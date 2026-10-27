{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Web (
  Handle (..),
  Config (..),
  withHandle,
  run,
  app,
) where

import Control.Applicative (empty, (<|>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Extra
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.UUID
import Data.UUID.V4
import qualified Logger
import Lucid hiding (for_)
import Network.HTTP.Types.Status (badRequest400, notFound404, unauthorized401)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Web.Scotty (ActionM, ScottyM, finish)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Cookie as Scotty

import Core
import Web.View

data Config = Config
  { cPort :: Maybe Int
  , cDomain :: Maybe T.Text
  }
  deriving (Show)

instance Semigroup Config where
  (<>) c1 c2 =
    Config
      { cPort = cPort c1 <|> cPort c2
      , cDomain = cDomain c1 <|> cDomain c2
      }

instance Monoid Config where
  mempty = Config empty empty

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Web.Config" $ \o ->
    Config
      <$> o A..:? "port"
      <*> o A..:? "domain"

data Handle = Handle
  { hConfig :: Config
  , hLogger :: Logger.Handle
  , hState :: TVar State
  }

withHandle ::
  Config ->
  Logger.Handle ->
  TVar State ->
  (Handle -> IO a) ->
  IO a
withHandle config logger state f =
  f $ Handle config logger state

run :: Handle -> IO ()
run h = Scotty.scotty port (app h)
 where
  port = fromMaybe 8000 $ cPort (hConfig h)

app :: Handle -> ScottyM ()
app h = do
  Scotty.get "/" $ do
    Scotty.html $ renderText mainView

  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.raw $ BL.fromStrict $(embedFile "assets/style.css")

  Scotty.get "/favicon.ico" $ do
    Scotty.setHeader "Content-Type" "image/x-icon"
    Scotty.raw $ BL.fromStrict $(embedFile "assets/favicon.ico")

  Scotty.post "/create" $ do
    hostName <- Scotty.formParam "name"
    gameId <- liftIO $ nextRandom
    host <- liftIO $ createPlayer hostName

    let game = initGame host
    liftIO $ atomically $ addGame (hState h) gameId game

    Scotty.setSimpleCookie "userId" (toText $ pId host)
    Scotty.redirect $ "/" <> (TL.fromStrict $ toText gameId)

  Scotty.post "/:gameId/reveal" $ do
    gameId <- getGameId
    userId <- getUserId

    result <- commit h $ withGame gameId (reveal userId)

    case result of
      Left _ -> Scotty.status badRequest400
      Right (_, game) ->
        liftIO $ sendUpdate (gPlayers game) gameId game

  Scotty.post "/:gameId/reset" $ do
    gameId <- getGameId
    userId <- getUserId

    result <- commit h $ withGame gameId (reset userId)

    case result of
      Left _ -> Scotty.status badRequest400
      Right (_, game) ->
        liftIO $ sendUpdate (gPlayers game) gameId game

  Scotty.get "/:gameId" $ do
    gameId <- getGameId

    (State games) <- liftIO $ readTVarIO (hState h)
    case M.lookup gameId games of
      Nothing -> Scotty.status notFound404
      Just game -> do
        mUserIdStr <- Scotty.getCookie "userId"
        let mUserId = fromText =<< mUserIdStr
        Scotty.html $ renderText $ gameView gameId mUserId game

  Scotty.post "/:gameId/join" $ do
    gameId <- getGameId
    name <- Scotty.formParam "name" -- TODO validate this?
    p <- liftIO $ createPlayer name

    result <- commit h $ withGame gameId (playerJoin p)

    case result of
      Left _ -> Scotty.status badRequest400
      Right (_, game) -> do
        liftIO $ sendUpdate (gPlayers game) gameId game
        Scotty.setSimpleCookie "userId" (toText $ pId p)
        Scotty.html $ renderText $ playerView (pId p) gameId game

  Scotty.post "/:gameId/vote/:vote" $ do
    userId <- getUserId
    gameId <- getGameId
    vote <- getVote

    result <- commit h $ withGame gameId (modifyVote userId vote)

    case result of
      Left _ -> Scotty.status badRequest400
      Right (_, game) ->
        liftIO $ sendUpdate (gPlayers game) gameId game

  Scotty.get "/:gameId/player" $ do
    userId <- getUserId
    gameId <- getGameId

    result <- commit h $ getChannel gameId userId -- TODO this is not really a commit

    case result of
      Left _ -> Scotty.status badRequest400
      Right (_, chan) -> Scotty.nested (eventSourceAppChan chan)

createPlayer :: Text -> IO Player
createPlayer name = do
  userId <- nextRandom
  chan <- newChan
  pure $ newPlayer name userId chan

addGame :: TVar State -> UUID -> Game -> STM ()
addGame stateRef gameId game = do
  modifyTVar stateRef (\(State games) -> State $ M.insert gameId game games)

sendUpdate :: [Player] -> UUID -> Game -> IO ()
sendUpdate players gameId game =
  for_ players $ \p ->
    let channel = pChan p
        d = [B.fromLazyByteString $ renderBS (playerView (pId p) gameId game)]
        event = ServerEvent Nothing Nothing d
     in writeChan channel event >> writeChan channel CloseEvent

getUserId :: ActionM UUID
getUserId = do
  mUserIdStr <- Scotty.getCookie "userId"
  case fromText =<< mUserIdStr of
    Nothing -> Scotty.status badRequest400 >> finish
    Just userId -> pure userId

getGameId :: ActionM UUID
getGameId = do
  mGameId <- fromText <$> Scotty.pathParam "gameId"
  case mGameId of
    Nothing -> Scotty.status badRequest400 >> finish
    Just gameId -> pure gameId

getVote :: ActionM Vote
getVote = do
  mVote <- mkVote <$> Scotty.pathParam "vote"
  case mVote of
    Nothing -> Scotty.status badRequest400 >> finish
    Just vote -> pure vote

commit :: Handle -> (State -> Either Err (State, a)) -> ActionM (Either Err (State, a))
commit h = liftIO . atomically . transact (hState h)

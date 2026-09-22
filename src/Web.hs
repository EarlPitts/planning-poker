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
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import Data.Foldable
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import qualified Logger
import Lucid hiding (for_)
import Network.HTTP.Types.Status (badRequest400, notFound404, unauthorized401)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Web.Scotty (ActionM, ScottyM)
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
    existingId <- Scotty.getCookie "id"
    state <- liftIO $ readTVarIO (hState h)
    let view = do
          game <- case state of
            Stopped -> Nothing
            InProgress game -> Just game
          pid <- fromText =<< existingId
          p <- findPlayer pid state
          let view' =
                if (sHost game == pId p)
                  then hostView
                  else playerView pid
          pure $ template "Planning Poker" $ view' state
    Scotty.html $ renderText $ fromMaybe (mainView state) view

  Scotty.get "/player/:id" $ do
    mId <- Scotty.pathParam "id"
    case fromString mId of
      Nothing -> Scotty.status badRequest400
      Just pId -> do
        state <- liftIO $ readTVarIO (hState h)
        case findPlayer pId state of
          Just p -> Scotty.nested (eventSourceAppChan (pChan p))
          Nothing -> Scotty.status notFound404

  Scotty.post "/host" $ do
    pName <- Scotty.formParam "name"
    pId <- liftIO nextRandom
    chan <- liftIO newChan
    let p = newPlayer pName pId chan
    state <- liftIO $ readTVarIO (hState h)
    case state of
      Stopped -> hostJoin h p
      InProgress _ -> playerJoin h p

  Scotty.post "/newPlayer" $ do
    pName <- Scotty.formParam "name"
    pId <- liftIO nextRandom
    chan <- liftIO newChan
    let p = newPlayer pName pId chan
    playerJoin h p

  Scotty.post "/vote/:id/:vote" $ do
    mVote <- mkVote <$> Scotty.pathParam "vote"
    case mVote of
      Nothing -> Scotty.status badRequest400
      Just pVote -> do
        mId <- Scotty.pathParam "id"
        case fromString mId of
          Nothing -> Scotty.status badRequest400
          Just pId -> do
            state <- liftIO $ atomically $ do
              modifyTVar' (hState h) (modifyPlayerVote pId pVote)
              readTVar (hState h)
            case state of
              Stopped -> Scotty.status $ badRequest400
              InProgress game -> do
                liftIO $ sendUpdate (sPlayers game) state
                Scotty.html $ renderText $ playerView pId state

  Scotty.post "/reveal" $ auth h $ \game -> do
    state <- liftIO $ atomically $ do
      modifyTVar' (hState h) reveal
      readTVar (hState h)
    liftIO $ sendUpdate (sPlayers game) state
    Scotty.html $ renderText $ hostView state

  Scotty.post "/reset" $ auth h $ \game -> do
    state <- liftIO $ atomically $ do
      modifyTVar' (hState h) reset
      readTVar (hState h)
    liftIO $ sendUpdate (sPlayers game) state
    Scotty.html $ renderText $ hostView state

  Scotty.post "/end" $ auth h $ \game -> do
    liftIO $ sendUpdate (sPlayers game) Stopped
    liftIO $ atomically $ modifyTVar' (hState h) end
    Scotty.html $ renderText $ hostView Stopped

  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.raw $ BL.fromStrict $(embedFile "assets/style.css")

  Scotty.get "/favicon.ico" $ do
    Scotty.setHeader "Content-Type" "image/x-icon"
    Scotty.raw $ BL.fromStrict $(embedFile "assets/favicon.ico")

auth :: Handle -> (Game -> ActionM ()) -> ActionM ()
auth h action = do
  mPid <- Scotty.getCookie "id"
  case fromText =<< mPid of
    Nothing -> Scotty.status unauthorized401
    Just pid -> do
      state <- liftIO $ readTVarIO (hState h)
      case state of
        Stopped -> Scotty.status unauthorized401
        InProgress g@Game{..} ->
          if (sHost == pid)
            then action g
            else Scotty.status unauthorized401

playerJoin :: Handle -> Player -> ActionM ()
playerJoin h p = do
  state <- liftIO $ atomically $ do
    modifyTVar' (hState h) (join p)
    readTVar (hState h)
  case state of
    Stopped -> Scotty.status $ badRequest400
    InProgress game -> do
      liftIO $ sendUpdate (sPlayers game) state
      liftIO $ Logger.logInfo (hLogger h) ("Player " <> (T.unpack $ pName p) <> " joined")
      Scotty.setSimpleCookie "id" (toText $ pId p)
      Scotty.html $ renderText (playerView (pId p) state)

hostJoin :: Handle -> Player -> ActionM ()
hostJoin h p = do
  let state = InProgress (Game [p] False (pId p))
  liftIO $ atomically $ writeTVar (hState h) state
  liftIO $ Logger.logInfo (hLogger h) ("Session started by " <> (T.unpack $ pName p))
  Scotty.setSimpleCookie "id" (toText $ pId p)
  Scotty.html $ renderText (hostView state)

sendUpdate :: [Player] -> State -> IO ()
sendUpdate players newState =
  for_ players $ \p ->
    let channel = pChan p
        d = [B.fromLazyByteString $ renderBS (playerView (pId p) newState)]
        event = ServerEvent Nothing Nothing d
     in writeChan channel event >> writeChan channel CloseEvent

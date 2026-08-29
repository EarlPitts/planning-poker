{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Web (
  Handle (..),
  Config (..),
  withHandle,
  run,
  app,
) where

import Control.Applicative (empty, (<|>))
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import qualified Logger
import Lucid
import Network.HTTP.Types.Status (badRequest400, notFound404, unauthorized401)
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
          pid <- fromText =<< existingId
          p <- findPlayer pid state
          pure $
            if (sHost state == pId p)
              then template "Planning Poker" $ hostView pid state
              else template "Planning Poker" $ playerView pid state
    Scotty.html $ renderText $ fromMaybe (mainView state) view

  Scotty.get "/player/:id" $ do
    mId <- Scotty.pathParam "id"
    case fromString mId of
      Nothing -> Scotty.status badRequest400
      Just pId -> do
        state <- liftIO $ readTVarIO (hState h)
        if (playerExists pId state)
          then Scotty.html $ renderText (playerView pId state)
          else Scotty.status notFound404

  Scotty.post "/host" $ do
    pName <- Scotty.formParam "name"
    pId <- liftIO nextRandom
    let p = newPlayer pName pId False
    state <- liftIO $ readTVarIO (hState h)
    case state of
      Stopped -> hostJoin h p
      InProgress _ _ _ -> playerJoin h p

  Scotty.post "/newPlayer" $ do
    pName <- Scotty.formParam "name"
    pId <- liftIO nextRandom
    let p = newPlayer pName pId False
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
            Scotty.html $
              renderText $
                if (sHost state == pId)
                  then hostView pId state
                  else playerView pId state

  Scotty.post "/reveal" $ auth h $ do
    state <- liftIO $ atomically $ do
      modifyTVar' (hState h) reveal
      readTVar (hState h)
    Scotty.html $ renderText $ hostView (sHost state) state

  Scotty.post "/reset" $ auth h $ do
    state <- liftIO $ atomically $ do
      modifyTVar' (hState h) reset
      readTVar (hState h)
    Scotty.html $ renderText $ hostView (sHost state) state

  Scotty.post "/end" $ auth h $ do
    state <- liftIO $ atomically $ do
      modifyTVar' (hState h) end
      readTVar (hState h)
    Scotty.html $ renderText $ hostView (sHost state) state

  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "assets/style.css"

auth :: Handle -> ActionM () -> ActionM ()
auth h action = do
  mPid <- Scotty.getCookie "id"
  case fromText =<< mPid of
    Nothing -> Scotty.status unauthorized401
    Just pid -> do
      state <- liftIO $ readTVarIO (hState h)
      case state of
        Stopped -> Scotty.status unauthorized401
        InProgress{..} ->
          if (sHost == pid)
            then action
            else Scotty.status unauthorized401

playerJoin :: Handle -> Player -> ActionM ()
playerJoin h p = do
  state <- liftIO $ atomically $ do
    modifyTVar' (hState h) (join p)
    readTVar (hState h)
  liftIO $ Logger.logInfo (hLogger h) ("Player " <> (T.unpack $ pName p) <> " joined")
  Scotty.setSimpleCookie "id" (toText $ pId p)
  Scotty.html $ renderText (playerView (pId p) state)

hostJoin :: Handle -> Player -> ActionM ()
hostJoin h p = do
  let state = InProgress [p] False (pId p)
  liftIO $ atomically $ writeTVar (hState h) state
  liftIO $ Logger.logInfo (hLogger h) ("Session started by " <> (T.unpack $ pName p))
  Scotty.setSimpleCookie "id" (toText $ pId p)
  Scotty.html $ renderText (hostView (pId p) state)

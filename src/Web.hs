{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Web (
  Handle (..),
  Config (..),
  withHandle,
  run,
) where

import Control.Applicative (empty, (<|>))
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import Data.Foldable
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import qualified Logger
import Lucid
import Lucid.Base (makeAttributes)
import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Cookie as Scotty

import Core

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

hxPost_, hxGet_, hxTarget_, hxSwap_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"
hxGet_ = makeAttributes "hx-get"
hxTarget_ = makeAttributes "hx-target"
hxTrigger_ = makeAttributes "hx-trigger"
hxSwap_ = makeAttributes "hx-swap"

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
              then hostView pid state
              else playerView pid state
    Scotty.html $ renderText $ fromMaybe (mainView state) view

  Scotty.get "/player/:id" $ do
    mId <- Scotty.pathParam "id"
    case fromString mId of
      Nothing -> Scotty.status badRequest400
      Just pId -> do
        state <- liftIO $ readTVarIO (hState h)
        Scotty.html $ renderText (playerView pId state)

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
    liftIO $ atomically $ modifyTVar' (hState h) reveal

  Scotty.post "/reset" $ auth h $ do
    liftIO $ atomically $ modifyTVar' (hState h) reset

  Scotty.post "/end" $ auth h $ do
    liftIO $ atomically $ modifyTVar' (hState h) end

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
      if (sHost state == pid)
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

template :: T.Text -> Html () -> Html ()
template title body = doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml title
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/style.css"]
    script_
      [ src_ "https://cdn.jsdelivr.net/npm/htmx.org@2.0.10/dist/htmx.min.js"
      , integrity_ "sha384-H5SrcfygHmAuTDZphMHqBJLc3FhssKjG7w/CeCpFReSfwBWDTKpkzPP8c+cLsK+V"
      , crossorigin_ "anonymous"
      ]
      ("" :: Text)
  body_ $ do
    -- header_ $ a_ [href_ "/"] "imageboard"
    body

mainView :: State -> Html ()
mainView s = template "Planning Poker" $ do
  case s of
    InProgress{..} ->
      div_ [id_ "parent-div"] $ do
        h2_ "Planning Poker"
        traverse_ (viewPlayer sIsRevealed) sPlayers
        form_ [hxPost_ "/newPlayer", hxTarget_ "#parent-div"] $ do
          input_ [name_ "name", type_ "text"]
          button_ "Join"
    Stopped ->
      div_ [id_ "parent-div"] $ do
        h2_ "Planning Poker"
        form_ [hxPost_ "/host", hxTarget_ "#parent-div"] $ do
          input_ [name_ "name", type_ "text"]
          button_ "Start new session as Host"

playerView :: UUID -> State -> Html ()
playerView _ Stopped = p_ "Session ended"
playerView id InProgress{..} = template "Planning Poker"
  $ div_
    [ id_ "parent-div"
    , hxGet_ $ "/player/" <> (toText id)
    , hxTrigger_ "every 2s"
    ]
  $ do
    h2_ "Planning Poker"
    let (p, rest) = partition (\p -> pId p == id) sPlayers
    traverse_ (viewPlayer True) p
    traverse_ (viewPlayer sIsRevealed) rest
    voteButtons id

voteButtons :: UUID -> Html ()
voteButtons pId = traverse_ (btn . T.pack . show) ([minBound .. maxBound] :: [Vote])
 where
  btn :: Text -> Html ()
  btn num =
    button_
      [ hxPost_ $ "/vote/" <> (toText pId) <> "/" <> num
      , hxTarget_ "#parent-div"
      ]
      (toHtml num)

hostView :: UUID -> State -> Html ()
hostView id state = do
  playerView id state
  button_
    [ hxPost_ "/reveal"
    , hxSwap_ "none"
    ]
    "Reveal"
  button_
    [ hxPost_ "/reset"
    , hxSwap_ "none"
    ]
    "Reset Votes"
  button_
    [ hxPost_ "/end"
    , hxSwap_ "none"
    ]
    "End"

viewPlayer :: Bool -> Player -> Html ()
viewPlayer revealed Player{..} = div_ $ do
  span_ $ toHtml pName
  ": "
  span_ $
    if revealed
      then viewVote pVote
      else "🂡"

viewVote :: Maybe Vote -> Html ()
viewVote Nothing = "🂠"
viewVote (Just v) = toHtml (show v)

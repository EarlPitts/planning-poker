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
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Foldable
import Data.IORef
import Data.Int (Int64)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.UUID
import Data.UUID.V4
import qualified Logger
import Lucid
import Lucid.Base (Attributes, makeAttributes)
import Network.HTTP.Types.Status (badRequest400, status404, unauthorized401)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import Poker

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
  , hState :: IORef State
  }

withHandle ::
  Config ->
  Logger.Handle ->
  IORef State ->
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
    liftIO $ Logger.logInfo (hLogger h) (show existingId)
    state <- liftIO $ readIORef (hState h)
    case existingId of
      Nothing -> do
        Scotty.html $ renderText (mainView state)
      Just mId -> do
        case fromText mId of
          Nothing -> Scotty.html $ renderText (mainView state)
          Just id -> do
            case findPlayer id state of
              Nothing -> Scotty.html $ renderText (mainView state)
              Just p -> do
                if (sHost state == pId p)
                  then Scotty.html $ renderText (hostView id state)
                  else Scotty.html $ renderText (playerView id state)

  Scotty.get "/player/:id" $ do
    mId <- Scotty.pathParam "id"
    case fromString mId of
      Nothing -> Scotty.status badRequest400
      Just pId -> do
        state <- liftIO $ readIORef (hState h)
        Scotty.html $ renderText (playerView pId state)

  Scotty.post "/host" $ do
    pName <- Scotty.formParam "name"
    pId <- liftIO nextRandom
    let p = newPlayer pName pId False
    state <- liftIO $ readIORef (hState h)
    case state of
      Stopped -> do
        let state = InProgress [p] False pId
        liftIO $ writeIORef (hState h) state
        Scotty.setSimpleCookie "id" (toText pId)
        Scotty.html $ renderText (hostView pId state)
      InProgress _ _ _ -> do
        liftIO $ modifyIORef (hState h) (join p)
        state <- liftIO $ readIORef (hState h)
        Scotty.setSimpleCookie "id" (toText pId)
        Scotty.html $ renderText (playerView pId state)

  Scotty.post "/newPlayer" $ do
    pName <- Scotty.formParam "name"
    pId <- liftIO nextRandom
    let p = newPlayer pName pId False
    liftIO $ modifyIORef (hState h) (join p)
    state <- liftIO $ readIORef (hState h)
    Scotty.setSimpleCookie "id" (toText pId)
    Scotty.html $ renderText (playerView pId state)

  Scotty.post "/vote/:id/:vote" $ do
    pVote <- mkVote <$> Scotty.pathParam "vote"
    case pVote of
      Nothing -> Scotty.status badRequest400
      Just v -> do
        mId <- Scotty.pathParam "id"
        case fromString mId of
          Nothing -> Scotty.status badRequest400
          Just pId -> do
            liftIO $ modifyIORef (hState h) (modifyPlayerVote pId v)
            state <- liftIO $ readIORef (hState h)
            if (sHost state == pId)
              then Scotty.html $ renderText (hostView pId state) -- TODO
              else Scotty.html $ renderText (playerView pId state)

  Scotty.post "/reveal" $ do
    liftIO $ modifyIORef (hState h) reveal

  Scotty.post "/reset" $ do
    liftIO $ modifyIORef (hState h) reset

  Scotty.post "/end" $ do
    liftIO $ modifyIORef (hState h) end

  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "assets/style.css"

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

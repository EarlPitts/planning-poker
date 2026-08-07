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
import Data.IORef
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Logger
import Lucid
import Network.HTTP.Types.Status (badRequest400, status404)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import Data.Foldable
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

run :: Handle -> IO ()
run h = Scotty.scotty port (app h)
 where
  port = fromMaybe 8000 $ cPort (hConfig h)

app :: Handle -> ScottyM ()
app h = do
  Scotty.get "/" $ do
    state <- liftIO $ readIORef (hState h)
    Scotty.html $ renderText (view state)

  Scotty.post "/newPlayer/:name" $ do
    pName <- Scotty.pathParam "name"
    let p = newPlayer pName
    liftIO $ modifyIORef (hState h) (join p)
  Scotty.post "/vote/:name/:vote" $ do
    pVote <- mkVote <$> Scotty.pathParam "vote"
    case pVote of
      Nothing -> Scotty.status badRequest400
      Just v -> do
        pName <- Scotty.pathParam "name"
        liftIO $ modifyIORef (hState h) (modifyPlayerVote pName v)
  Scotty.post "/reveal" $ do
    liftIO $ modifyIORef (hState h) reveal
  Scotty.post "/reset" $ do
    liftIO $ modifyIORef (hState h) reset

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

view :: State -> Html ()
view State{..} = template "Planning Poker" $ do
  h2_ "Planning Poker"
  traverse_ (viewPlayers sIsRevealed) sPlayers

viewPlayers :: Bool -> Player -> Html ()
viewPlayers revealed Player{..} = div_ $ do
  span_ $ toHtml pName
  ": "
  span_ $
    if revealed
      then viewVote pVote
      else "🂡"

viewVote :: Maybe Vote -> Html ()
viewVote Nothing = "🂠"
viewVote (Just v) = toHtml (show v)

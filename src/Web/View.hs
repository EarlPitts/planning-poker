module Web.View where

import Data.Foldable
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Lucid
import Lucid.Base (makeAttributes)

import Control.Monad (when)
import Core

hxPost_, hxGet_, hxTarget_, hxTrigger_, hxSwap_, hxExt_, sseConnect_, sseSwap_, hxPushUrl_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"
hxGet_ = makeAttributes "hx-get"
hxTarget_ = makeAttributes "hx-target"
hxTrigger_ = makeAttributes "hx-trigger"
hxSwap_ = makeAttributes "hx-swap"
hxExt_ = makeAttributes "hx-ext"
sseConnect_ = makeAttributes "sse-connect"
sseSwap_ = makeAttributes "sse-swap"
hxPushUrl_ = makeAttributes "hx-push-url"

template :: T.Text -> Html () -> Html ()
template title body = doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml title
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/style.css"]
    script_
      [ src_ "https://cdn.jsdelivr.net/npm/htmx.org@2.0.10/dist/htmx.min.js"
      , crossorigin_ "anonymous"
      ]
      ("" :: Text)
    script_
      [ src_ "https://cdn.jsdelivr.net/npm/htmx-ext-sse@2.2.4/dist/sse.min.js"
      , crossorigin_ "anonymous"
      ]
      ("" :: Text)
  body_ [hxExt_ "sse"] $ do
    h2_ "Planning Poker"
    -- header_ $ a_ [href_ "/"] "imageboard"
    body

mainView :: Html ()
mainView = template "Planning Poker" $ do
  -- div_ [id_ "main-view"] $ do
  form_ [method_ "post", action_ "/create"] $ do
    input_ [name_ "name", type_ "text", placeholder_ "Enter your name..."]
    button_ "Start new session as Host"

gameView :: UUID -> Maybe UUID -> Game -> Html ()
gameView gameId mUserId game = template "Planning Poker" $ do
  div_ [id_ "#game-view"] $ case mUserId of
    Nothing -> joinView gameId game
    Just userId -> do
      connectionView userId gameId game
      when (userId == gHost game) (hostView gameId)

joinView :: UUID -> Game -> Html ()
joinView gameId Game{..} =
  div_ [id_ "join-view"] $ do
    traverse_ (viewPlayer gIsRevealed) gPlayers
    form_
      [ hxPost_ $ "/" <> (toText gameId) <> "/join"
      , hxTarget_ "#join-view"
      , hxSwap_ "outerHTML"
      ]
      $ do
        input_ [name_ "name", type_ "text", placeholder_ "Enter your name..."]
        button_ "Join"

connectionView :: UUID -> UUID -> Game -> Html ()
connectionView userId gameId Game{..} =
  div_
    [ sseConnect_ $ (toText gameId) <> "/player"
    , sseSwap_ "message"
    , hxSwap_ "player-view"
    ]
    $ playerView userId gameId Game{..}

playerView :: UUID -> UUID -> Game -> Html ()
playerView userId gameId Game{..} = div_ [id_ "player-view"] $ do
  let (p, rest) = partition (\p -> pId p == userId) gPlayers
  div_ [class_ "player-list"] $ do
    traverse_ (viewPlayer True) p
    traverse_ (viewPlayer gIsRevealed) rest
  when gIsRevealed $ statView gPlayers
  voteButtons gameId

statView :: [Player] -> Html ()
statView ps = div_ [] $ do
  p_ $ "Average: " <> (toHtml $ show $ voteAverage ps)
  case fight ps of
    Nothing -> pure ()
    Just (top, bot) ->
      p_ $ "Fight: " <> (toHtml $ show $ top) <> " vs " <> (toHtml $ show $ bot)

voteButtons :: UUID -> Html ()
voteButtons gameId = traverse_ (btn . T.pack . show) ([minBound .. maxBound] :: [Vote])
 where
  btn :: Text -> Html ()
  btn num =
    button_
      [ hxPost_ $ "/" <> (toText gameId) <> "/vote/" <> num
      , hxSwap_ "none"
      ]
      (toHtml num)

hostView :: UUID -> Html ()
hostView gameId = div_ [id_ "host-view"] $ do
  div_ $ do
    button_
      [ hxPost_ $ (toText gameId) <> "/reveal"
      , hxSwap_ "none"
      ]
      "Reveal"
    button_
      [ hxPost_ $ (toText gameId) <> "/reset"
      , hxSwap_ "none"
      ]
      "Reset Votes"
  div_ $
    button_
      [ type_ "button"
      , onclick_ "navigator.clipboard.writeText(window.location.href)"
      ]
      "Copy Link to Clipboard"

viewPlayer :: Bool -> Player -> Html ()
viewPlayer revealed Player{..} = div_ [class_ "player"] $ do
  span_ $ toHtml pName
  ":"
  span_ [class_ "vote"] $
    case (revealed, pVote) of
      (True, _) -> viewVote pVote
      (False, Nothing) -> "🂠"
      (False, _) -> "🂡"

viewVote :: Maybe Vote -> Html ()
viewVote Nothing = "🂠"
viewVote (Just v) = toHtml (show v)

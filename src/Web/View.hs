module Web.View where

import Data.Foldable
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Lucid
import Lucid.Base (makeAttributes)

import Core

hxPost_, hxGet_, hxTarget_, hxTrigger_, hxSwap_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"
hxGet_ = makeAttributes "hx-get"
hxTarget_ = makeAttributes "hx-target"
hxTrigger_ = makeAttributes "hx-trigger"
hxSwap_ = makeAttributes "hx-swap"

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
playerView id InProgress{..} = template "Planning Poker" $ div_
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
    , hxTarget_ "#parent-div"
    ]
    "Reveal"
  button_
    [ hxPost_ "/reset"
    , hxTarget_ "#parent-div"
    ]
    "Reset Votes"
  button_
    [ hxPost_ "/end"
    , hxTarget_ "#parent-div"
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

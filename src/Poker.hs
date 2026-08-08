module Poker where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T

data Vote = One | Two deriving (Eq, Show)

data Player = Player
  { pVote :: Maybe Vote
  , pName :: T.Text
  , pIsHost :: Bool
  }
  deriving (Eq, Show)

data State = State
  { sPlayers :: [Player]
  , sIsRevealed :: Bool
  }
  deriving (Eq, Show)

mkVote :: String -> Maybe Vote
mkVote "1" = Just One
mkVote "2" = Just Two
mkVote _ = Nothing

initState :: State
initState = State [] False

testState =
  State
    [ Player (Just One) "Bela" True
    , Player (Just Two) "Jani" False
    , Player Nothing "Jeno" False
    ]
    False

join :: Player -> State -> State
join p s = s{sPlayers = p : sPlayers s}

newPlayer :: Text -> Bool -> Player
newPlayer = Player Nothing 

modifyPlayerVote :: Text -> Vote -> State -> State
modifyPlayerVote name v s@State{..} =
  s{sPlayers = update <$> sPlayers}
 where
  update p = if pName p == name then vote p v else p

vote :: Player -> Vote -> Player
vote p v = p{pVote = Just v}

reveal :: State -> State
reveal s = s{sIsRevealed = True}

reset :: State -> State
reset s = s{sIsRevealed = False, sPlayers = resetVote <$> sPlayers s}

resetVote :: Player -> Player
resetVote p = p{pVote = Nothing}

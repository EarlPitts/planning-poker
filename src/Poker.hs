module Poker where

import Data.Text (Text)
import qualified Data.Text as T

data Vote
  = Instant
  | Quarter
  | Half
  | One
  | OneAndHalf
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Enum, Bounded)

instance Show Vote where
  show Instant = "0.1"
  show Quarter = "0.25"
  show Half = "0.5"
  show One = "1"
  show OneAndHalf = "1.5"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"

data Player = Player
  { pVote :: Maybe Vote
  , pName :: T.Text
  , pIsHost :: Bool
  }
  deriving (Eq, Show)

data State
  = Stopped
  | InProgress
      { sPlayers :: [Player]
      , sIsRevealed :: Bool
      , sHost :: T.Text
      }
  deriving (Eq, Show)

mkVote :: String -> Maybe Vote
mkVote "1" = Just One
mkVote "2" = Just Two
mkVote _ = Nothing

initState :: State
initState = Stopped

testState =
  InProgress
    [ Player (Just One) "Bela" True
    , Player (Just Two) "Jani" False
    , Player Nothing "Jeno" False
    ]
    False
    "Bela"

join :: Player -> State -> State
join _ Stopped = Stopped
join p s = s{sPlayers = p : sPlayers s}

newPlayer :: Text -> Bool -> Player
newPlayer = Player Nothing

modifyPlayerVote :: Text -> Vote -> State -> State
modifyPlayerVote _ _ Stopped = Stopped
modifyPlayerVote name v s@InProgress{..} =
  s{sPlayers = update <$> sPlayers}
 where
  update p = if pName p == name then vote p v else p

vote :: Player -> Vote -> Player
vote p v = p{pVote = Just v}

reveal :: State -> State
reveal Stopped = Stopped
reveal s = s{sIsRevealed = True}

reset :: State -> State
reset Stopped = Stopped
reset s = s{sIsRevealed = False, sPlayers = resetVote <$> sPlayers s}

end :: State -> State
end _ = Stopped

resetVote :: Player -> Player
resetVote p = p{pVote = Nothing}

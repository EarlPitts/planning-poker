module Core where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID

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
  , pId :: UUID
  , pIsHost :: Bool
  }
  deriving (Eq, Show)

data State
  = Stopped
  | InProgress
      { sPlayers :: [Player]
      , sIsRevealed :: Bool
      , sHost :: UUID
      }
  deriving (Eq, Show)

mkVote :: String -> Maybe Vote
mkVote "0.1" = Just Instant
mkVote "0.25" = Just Quarter
mkVote "0.5" = Just Half
mkVote "1" = Just One
mkVote "1.5" = Just OneAndHalf
mkVote "2" = Just Two
mkVote "3" = Just Three
mkVote "4" = Just Four
mkVote "5" = Just Five
mkVote _ = Nothing

initState :: State
initState = Stopped

join :: Player -> State -> State
join _ Stopped = Stopped
join p s = s{sPlayers = p : sPlayers s}

newPlayer :: Text -> UUID -> Bool -> Player
newPlayer = Player Nothing

findPlayer :: UUID -> State -> Maybe Player
findPlayer _ Stopped = Nothing
findPlayer id InProgress{..} = find (\p -> pId p == id) sPlayers

modifyPlayerVote :: UUID -> Vote -> State -> State
modifyPlayerVote _ _ Stopped = Stopped
modifyPlayerVote id v s@InProgress{..} =
  s{sPlayers = update <$> sPlayers}
 where
  update p = if pId p == id then vote p v else p

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

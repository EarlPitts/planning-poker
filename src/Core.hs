{-# LANGUAGE LambdaCase #-}

module Core where

import Control.Concurrent.Chan
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Network.Wai.EventSource (ServerEvent)

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

toDouble :: Vote -> Double
toDouble = \case
  Instant -> 0.1
  Quarter -> 0.25
  Half -> 0.5
  One -> 1
  OneAndHalf -> 1.5
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5

instance Show Vote where
  show = show . toDouble

data Player = Player
  { pVote :: Maybe Vote
  , pName :: T.Text
  , pId :: UUID
  , pChan :: Chan ServerEvent
  }
  deriving (Eq)

instance Show Player where
  show Player{..} = "Player: " <> T.unpack pName

data State
  = Stopped
  | InProgress
      { sPlayers :: [Player]
      , sIsRevealed :: Bool
      , sHost :: UUID
      }
  deriving (Eq)

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

newPlayer :: Text -> UUID -> Chan ServerEvent -> Player
newPlayer = Player Nothing

findPlayer :: UUID -> State -> Maybe Player
findPlayer _ Stopped = Nothing
findPlayer id InProgress{..} = find (\p -> pId p == id) sPlayers

playerExists :: UUID -> State -> Bool
playerExists uuid state = maybe False (const True) $ findPlayer uuid state

gameEnded :: State -> Bool
gameEnded state = state == Stopped

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

voteAverage :: State -> Double
voteAverage Stopped = 0
voteAverage InProgress{..} = vSum / fromIntegral pNum
 where
  votes = catMaybes $ fmap pVote sPlayers
  vSum = sum $ fmap toDouble votes
  pNum = length votes

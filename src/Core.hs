{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Core where

import Control.Concurrent.Chan
import Data.List (find, groupBy, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Network.Wai.EventSource (ServerEvent)
import System.Random

data Vote
  = Instant
  | Quarter
  | Half
  | ThreeQuarters
  | One
  | OneAndHalf
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Enum, Ord, Bounded)

toDouble :: Vote -> Double
toDouble = \case
  Instant -> 0.1
  Quarter -> 0.25
  Half -> 0.5
  ThreeQuarters -> 0.75
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
  show Player{..} = T.unpack pName

data State
  = Stopped
  | InProgress Game
  deriving (Eq, Show)

data Game = Game
  { sPlayers :: [Player]
  , sIsRevealed :: Bool
  , sHost :: UUID
  }
  deriving (Eq, Show)

data Err = AuthErr | UserErr deriving (Show, Eq)

data JoinResult = HostJoined | PlayerJoined deriving (Show, Eq)

data Outcome a = Outcome
  { oState :: State
  , oNotify :: [Player]
  , oResult :: a
  }
  deriving (Show, Eq)

mkVote :: String -> Maybe Vote
mkVote "0.1" = Just Instant
mkVote "0.25" = Just Quarter
mkVote "0.5" = Just Half
mkVote "0.75" = Just ThreeQuarters
mkVote "1.0" = Just One
mkVote "1.5" = Just OneAndHalf
mkVote "2.0" = Just Two
mkVote "3.0" = Just Three
mkVote "4.0" = Just Four
mkVote "5.0" = Just Five
mkVote _ = Nothing

initState :: State
initState = Stopped

playerJoin :: Player -> State -> Either Err (Outcome JoinResult)
playerJoin p = \case
  Stopped -> Right $ Outcome (InProgress (Game [p] False (pId p))) [] HostJoined
  InProgress game ->
    if any (\o -> pName p == pName o) (sPlayers game)
      then Left UserErr
      else
        let newState = InProgress game{sPlayers = p : sPlayers game}
         in Right $ Outcome newState (sPlayers game) PlayerJoined

newPlayer :: Text -> UUID -> Chan ServerEvent -> Player
newPlayer = Player Nothing

findPlayer :: UUID -> State -> Maybe Player
findPlayer _ Stopped = Nothing
findPlayer id (InProgress Game{..}) = find (\p -> pId p == id) sPlayers

getPlayers :: State -> [Player]
getPlayers Stopped = []
getPlayers (InProgress Game{..}) = sPlayers

playerExists :: UUID -> State -> Bool
playerExists uuid state = maybe False (const True) $ findPlayer uuid state

gameEnded :: State -> Bool
gameEnded state = state == Stopped

modifyPlayerVote :: UUID -> Vote -> State -> Either Err (Outcome ())
modifyPlayerVote uuid v = \case
  Stopped -> Left UserErr
  s@(InProgress g@Game{..}) ->
    if playerExists uuid s
      then
        let update p = if pId p == uuid then vote p v else p
         in Right $ Outcome (InProgress g{sPlayers = update <$> sPlayers}) sPlayers ()
      else Left UserErr

vote :: Player -> Vote -> Player
vote p v = p{pVote = Just v}

reveal :: UUID -> State -> Either Err (Outcome ())
reveal _ Stopped = Left UserErr
reveal uuid (InProgress g) =
  if uuid == sHost g
    then Right $ Outcome (InProgress g{sIsRevealed = True}) (sPlayers g) ()
    else Left AuthErr

reset :: UUID -> State -> Either Err (Outcome ())
reset _ Stopped = Left UserErr
reset uuid (InProgress g) =
  if uuid == sHost g
    then
      Right $
        Outcome
          ( InProgress
              g
                { sIsRevealed = False
                , sPlayers = resetVote <$> sPlayers g
                }
          )
          (sPlayers g)
          ()
    else Left AuthErr

end :: UUID -> State -> Either Err (Outcome ())
end _ Stopped = Left UserErr
end uuid (InProgress g) =
  if uuid == sHost g
    then Right $ Outcome Stopped (sPlayers g) ()
    else Left AuthErr

resetVote :: Player -> Player
resetVote p = p{pVote = Nothing}

voteAverage :: [Player] -> Double
voteAverage players = vSum / fromIntegral pNum
 where
  votes = catMaybes $ fmap pVote players
  vSum = sum $ fmap toDouble votes
  pNum = length votes

fight :: [Player] -> Maybe (Player, Player)
fight players =
  if length groups < 2
    then Nothing
    else Just (pick top, pick bot)
 where
  pSorted = sortOn pVote players
  groups = filter (\ps -> Nothing `notElem` fmap pVote ps) $ groupBy (\p1 p2 -> pVote p1 == pVote p2) pSorted
  top = last groups
  bot = head groups
  seed = sum $ fmap toDouble $ catMaybes $ fmap pVote players
  pick l =
    let (i, _) = uniformR (0, length l - 1) (mkStdGen $ fromIntegral @Int $ floor seed)
     in l !! i

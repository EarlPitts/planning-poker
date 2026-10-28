{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Core where

import Control.Concurrent.Chan
import Control.Monad (unless, when)
import Data.List (find, groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
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
  , pBanned :: Bool
  , pName :: T.Text
  , pId :: UUID
  , pChan :: Chan ServerEvent
  }
  deriving (Eq)

instance Show Player where
  show Player{..} = T.unpack pName

data State = State
  { sGames :: Map UUID Game
  }
  deriving (Eq, Show)

data Game = Game
  { gPlayers :: [Player] -- TODO make this a NEL
  , gIsRevealed :: Bool
  , gHost :: UUID
  }
  deriving (Eq, Show)

data Err = AuthErr | UserErr deriving (Show, Eq)

data JoinResult = HostJoined | PlayerJoined deriving (Show, Eq)

data Outcome a = Outcome
  { oGame :: Game
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
initState = State mempty

withGame :: UUID -> (Game -> Either Err Game) -> State -> Either Err (State, Game)
withGame gameId f (State games) = case M.lookup gameId games of
  Nothing -> Left UserErr
  Just game -> case f game of
    Left err -> Left err
    Right newGame -> Right $ (State (M.insert gameId newGame games), newGame)

-- playerJoin :: Player -> Game -> Either Err Game
-- playerJoin p game@Game{..} =
--   if any (\o -> pName p == pName o) gPlayers
--     then Left UserErr
--     else Right $ game{gPlayers = p : gPlayers}

findGame :: UUID -> State -> Maybe Game
findGame uuid (State games) = games M.!? uuid

initGame :: Player -> Game
initGame host =
  Game
    { gPlayers = [host]
    , gIsRevealed = False
    , gHost = pId host
    }

newPlayer :: Text -> UUID -> Chan ServerEvent -> Player
newPlayer = Player Nothing False

findPlayer :: UUID -> Game -> Maybe Player
findPlayer uuid Game{..} = find (\p -> pId p == uuid) gPlayers

getPlayers :: Game -> [Player]
getPlayers Game{..} = gPlayers

playerExists :: UUID -> Game -> Bool
playerExists uuid game = maybe False (const True) $ findPlayer uuid game

changeVote :: Player -> Vote -> Player
changeVote p v = p{pVote = Just v}

hostAuth :: UUID -> Game -> Either Err ()
hostAuth userId game = when (userId /= gHost game) (Left UserErr)

reset :: UUID -> Game -> Either Err Game
reset userId game = do
  hostAuth userId game
  pure
    game
      { gIsRevealed = False
      , gPlayers = resetVote <$> gPlayers game
      }

reveal :: UUID -> Game -> Either Err Game
reveal userId game = do
  hostAuth userId game
  pure game{gIsRevealed = True}

kick :: UUID -> UUID -> Game -> Either Err Game
kick userId userToKick game = do
  hostAuth userId game
  pure
    game
      { gPlayers =
          fmap
            ( \p ->
                if pId p == userToKick
                  then p{pBanned = True, pVote = Nothing}
                  else p
            )
            (gPlayers game)
      }

modifyVote :: UUID -> Vote -> Game -> Either Err Game
modifyVote userId vote game = do
  p <- maybe (Left UserErr) Right $ findPlayer userId game
  when (pBanned p) $ Left UserErr
  let newPlayer = changeVote p vote
  let ps = fmap (\p' -> if pId p' == pId p then newPlayer else p') (gPlayers game)
  pure game{gPlayers = ps}

playerJoin :: Player -> Game -> Either Err Game
playerJoin player game = do
  when (any (\p -> pName p == pName player) (gPlayers game)) (Left UserErr)
  pure game{gPlayers = player : gPlayers game}

resetVote :: Player -> Player
resetVote p = p{pVote = Nothing}

getChannel :: UUID -> UUID -> State -> Either Err (State, Chan ServerEvent)
getChannel gameId userId (State games) = case M.lookup gameId games of
  Nothing -> Left UserErr
  Just game -> case findPlayer userId game of
    Nothing -> Left UserErr
    Just p -> Right $ (State games, pChan p)

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

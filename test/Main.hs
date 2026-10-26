{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Core
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.UUID (fromString, toASCIIBytes)
import GHC.IO (unsafePerformIO)
import qualified Logger
import Network.HTTP.Types (hContentType, hCookie, methodGet, methodPost)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Network.Wai (Application, Request (..), requestHeaders, requestMethod)
import Network.Wai.EventSource.EventStream
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Property (failed, succeeded)
import Web (Config (..), app, withHandle)
import qualified Web.Scotty as Scotty

instance Arbitrary Vote where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary (Chan ServerEvent) where
  arbitrary = pure dummyChannel

instance Arbitrary Player where
  arbitrary =
    Player
      <$> arbitrary
      <*> (T.pack <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

dummyChannel :: Chan ServerEvent
dummyChannel = unsafePerformIO newChan

postForm :: Application -> ByteString -> [(ByteString, ByteString)] -> IO SResponse
postForm application path params =
  runSession (srequest (SRequest req body)) application
 where
  body = LBC.fromStrict (renderSimpleQuery False params)
  req =
    (setPath defaultRequest path)
      { requestMethod = methodPost
      , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
      }

joinRequest :: Application -> ByteString -> IO SResponse
joinRequest application name =
  postForm application "/join" [("name", name)]

fakeGameId = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c2")
fakeHostId = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c2")
testInitState = State $ M.fromList [(fakeGameId, Game [] False fakeHostId)]

main :: IO ()
main = hspec $ do
  testsCore
  -- testsRoute

testsCore :: Spec
testsCore = do
  it "joining with new name succeeds" $ do
    property $ \p1 p2 ->
      pName p1
        /= pName p2
          ==> case playerJoin fakeGameId p1 testInitState of
            Right (newState, _) ->
              case playerJoin fakeGameId p2 newState of
                Right (_, _) -> succeeded
                _ -> failed
            _ -> failed

  it "joining with existing name fails" $ do
    property $ \p1 p2 ->
      case playerJoin fakeGameId p1 testInitState of
        Right (newState, _) ->
          case playerJoin fakeGameId p2{pName = pName p1} newState of
            Left UserErr -> succeeded
            _ -> failed
        _ -> failed

  it "finds existing player" $ do
    property $ \player others revealed host ->
      let game =
            Game
              { gPlayers = (player : others)
              , gIsRevealed = revealed
              , gHost = host
              }
       in findPlayer (pId player) game == Just player

  it "doesn't find non-existent player" $ do
    property $ \player others revealed host ->
      let others' = filter (\o -> (pId o) /= (pId player)) others
          game =
            Game
              { gPlayers = others'
              , gIsRevealed = revealed
              , gHost = host
              }
       in findPlayer (pId player) game == Nothing

  it "cannot modify vote in non-existent game" $ do
    property $ \gameId userId vote ->
      modifyVote gameId userId vote (State $ M.fromList []) == Left UserErr

  it "voting for player works" $ do
    property $ \player others vote revealed host -> do
      let others' = filter (\o -> (pId o) /= (pId player)) others
          state =
            State $
              M.fromList
                [
                  ( fakeGameId
                  , Game
                      { gPlayers = (player : others')
                      , gIsRevealed = revealed
                      , gHost = host
                      }
                  )
                ]

      let Right (_, game) = modifyVote fakeGameId (pId player) vote state

      let finalVote = pVote <$> findPlayer (pId player) game
      finalVote === Just (Just vote)

  it "computing the average of votes works" $ do
    let votes = [Just One, Just Two, Just Instant, Nothing, Just OneAndHalf, Nothing, Just Five]
        uuid = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c2")
        players = fmap (\v -> Player v "test" uuid dummyChannel) votes

    let average = voteAverage players

    average == 1.92

  it "getting the top and bottom voter works" $ do
    let votes = [Just One, Just Two, Just Instant, Nothing, Just OneAndHalf, Nothing, Just Five]
        uuid = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c2")
        players = fmap (\v -> Player v "test" uuid dummyChannel) votes

    let Just (top, bot) = fight players

    pVote top `shouldBe` Just Five
    pVote bot `shouldBe` Just Instant

mkApp :: State -> IO Application
mkApp state = do
  s <- newTVarIO state
  let config = Web.Config Nothing Nothing
  Logger.withHandle (Logger.Config (Just Logger.Error)) $ \logger ->
    Web.withHandle config logger s (Scotty.scottyApp . app)

mkApp' :: TVar State -> IO Application
mkApp' s = do
  let config = Web.Config Nothing Nothing
  Logger.withHandle (Logger.Config (Just Logger.Error)) $ \logger ->
    Web.withHandle config logger s (Scotty.scottyApp . app)

-- testsRoute :: Spec
-- testsRoute = do
--   let existingUUID = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c2")
--       nonExistingUUID = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c3")
--       state =
--         State $
--           M.fromList
--             [
--               ( fakeGameId
--               , Game
--                   { gPlayers = [Player Nothing "Jon Doe" existingUUID dummyChannel]
--                   , gIsRevealed = False
--                   , gHost = existingUUID
--                   }
--               )
--             ]
--
--   describe "GET /" $ do
--     with (mkApp state) $ do
--       it "response with 200 when no game is in progress" $ do
--         get "/" `shouldRespondWith` 200
--
--
--   describe "GET /player" $ do
--     with (mkApp state) $ do
--       it "response with 401 when id is not valid UUID" $ do
--         request methodGet "/player" [(hCookie, "playerId=" <> "not-a-uuid")] ""
--           `shouldRespondWith` 401
--
--       it "response with 401 when player with given id is not found" $ do
--         request methodGet "/player" [(hCookie, "playerId=" <> (toASCIIBytes nonExistingUUID))] ""
--           `shouldRespondWith` 401

  -- describe "POST /join" $ do
  --   it "no race condition while joining" $ do
  --     stateRef <- liftIO $ newTVarIO initState
  --     application <- mkApp' stateRef
  --     let names = (BC.pack . show) <$> ([1 .. 100] :: [Int])
  --     _ <- mapConcurrently_ (joinRequest application) names
  --     state <- readTVarIO stateRef
  --     (length $ getPlayers state) `shouldBe` 100

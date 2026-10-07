{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Core
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.UUID (fromString, toASCIIBytes)
import GHC.IO (unsafePerformIO)
import qualified Logger
import Network.HTTP.Types (hContentType, methodPost)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Network.Wai (Application, Request (..), requestHeaders, requestMethod)
import Network.Wai.EventSource.EventStream
import Network.Wai.Test
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

main :: IO ()
main = hspec $ do
  pure ()
  testsCore
  testsRoute

testsCore :: Spec
testsCore = do
  it "joining when no game is running starts it as host" $ do
    property $ \p ->
      case playerJoin p Stopped of
        Right (InProgress _, HostJoined) -> succeeded
        _ -> failed

  it "joining with new name succeeds" $ do
    property $ \p1 p2 ->
      pName p1
        /= pName p2
          ==> case playerJoin p1 Stopped of
            Right (state, HostJoined) ->
              case playerJoin p2 state of
                Right (_, PlayerJoined) -> succeeded
                _ -> failed
            _ -> failed

  it "joining with existing name fails" $ do
    property $ \p1 p2 ->
      case playerJoin p1 Stopped of
        Right (state, HostJoined) ->
          case playerJoin p2{pName = pName p1} state of
            Left UserErr -> succeeded
            _ -> failed
        _ -> failed

  it "no players in stopped game" $ do
    property $ \uuid ->
      findPlayer uuid Stopped == Nothing

  it "finds existing player" $ do
    property $ \player others revealed host ->
      let state =
            InProgress
              Game
                { sPlayers = (player : others)
                , sIsRevealed = revealed
                , sHost = host
                }
       in findPlayer (pId player) state == Just player

  it "doesn't find non-existent player" $ do
    property $ \player others revealed host ->
      let others' = filter (\o -> (pId o) /= (pId player)) others
          state =
            InProgress
              Game
                { sPlayers = others'
                , sIsRevealed = revealed
                , sHost = host
                }
       in findPlayer (pId player) state == Nothing

  it "cannot modify vote in stopped game" $ do
    property $ \uuid v ->
      modifyPlayerVote uuid v Stopped == Stopped

  it "voting for player works" $ do
    property $ \player others v revealed host -> do
      let others' = filter (\o -> (pId o) /= (pId player)) others
          state =
            InProgress
              Game
                { sPlayers = (player : others')
                , sIsRevealed = revealed
                , sHost = host
                }

      let resultState = modifyPlayerVote (pId player) v state

      let finalVote = pVote <$> findPlayer (pId player) resultState
      finalVote === Just (Just v)

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

testsRoute :: Spec
testsRoute = do
  let existingUUID = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c2")
      nonExistingUUID = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c3")
      runningState =
        InProgress
          Game
            { sPlayers = [Player Nothing "Jon Doe" existingUUID dummyChannel]
            , sIsRevealed = False
            , sHost = existingUUID
            }

  describe "GET /" $ do
    with (mkApp Stopped) $ do
      it "response with 200 when no game is in progress" $ do
        get "/" `shouldRespondWith` 200

  describe "GET /player/:id" $ do
    with (mkApp runningState) $ do
      it "response with 400 when id is not valid UUID" $ do
        get "/player/not-uuid" `shouldRespondWith` 400

      it "response with 404 when player with given id is not found" $ do
        get ("/player/" <> toASCIIBytes nonExistingUUID)
          `shouldRespondWith` 404

  describe "POST /join" $ do
    it "no race condition while joining" $ do
      stateRef <- liftIO $ newTVarIO Stopped
      application <- mkApp' stateRef
      let names = (BC.pack . show) <$> ([1 .. 100] :: [Int])
      _ <- mapConcurrently_ (joinRequest application) names
      state <- readTVarIO stateRef
      (length $ getPlayers state) `shouldBe` 100

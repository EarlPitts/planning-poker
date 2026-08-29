{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Core
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.UUID (fromString, toASCIIBytes)
import GHC.Conc (newTVarIO)
import qualified Logger
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck
import Test.QuickCheck.Instances.UUID ()
import Web (Config (..), app, withHandle)
import qualified Web.Scotty as Scotty

instance Arbitrary Vote where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Player where
  arbitrary =
    Player
      <$> arbitrary
      <*> (T.pack <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

main :: IO ()
main = hspec $ do
  testsCore
  testsRoute

testsCore :: Spec
testsCore = do
  it "cannot join stopped game" $ do
    property $ \player ->
      join player Stopped == Stopped

  it "no players in stopped game" $ do
    property $ \uuid ->
      findPlayer uuid Stopped == Nothing

  it "finds existing player" $ do
    property $ \player others revealed host ->
      let state =
            InProgress
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
              { sPlayers = (player : others')
              , sIsRevealed = revealed
              , sHost = host
              }

      let resultState = modifyPlayerVote (pId player) v state

      let finalVote = pVote <$> findPlayer (pId player) resultState
      finalVote === Just (Just v)

mkApp :: State -> IO Application
mkApp state = do
  s <- newTVarIO state
  let config = Web.Config Nothing Nothing
  Logger.withHandle (Logger.Config (Just Logger.Error)) $ \logger ->
    Web.withHandle config logger s (Scotty.scottyApp . app)

testsRoute :: Spec
testsRoute = do
  describe "GET /" $ do
    with (mkApp Stopped) $ do
      it "response with 200 when no game is in progress" $ do
        get "/" `shouldRespondWith` 200

  describe "GET /player/:id" $ do
    with (mkApp Stopped) $ do
      it "response with 400 when id is not valid UUID" $ do
        get "/player/not-uuid" `shouldRespondWith` 400

      it "response with 404 when player with given id is not found" $ do
        get "/player/902d870d-11b3-46cd-8296-6a9cf1a376c2"
          `shouldRespondWith` 404

    let uuid = fromJust (fromString "902d870d-11b3-46cd-8296-6a9cf1a376c2")
        runningState =
          InProgress
            { sPlayers = [Player Nothing "Jon Doe" uuid False]
            , sIsRevealed = False
            , sHost = uuid
            }
    with (mkApp runningState) $ do
      it "response with 200 when player exists" $ do
        get ("/player/" <> toASCIIBytes uuid)
          `shouldRespondWith` 200

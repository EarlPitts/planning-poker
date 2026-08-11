{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PlanningPoker (
  Config (..),
  main,
) where

import qualified Data.Aeson as A
import Control.Concurrent.STM
import Data.Version (showVersion)
import qualified Data.Yaml as Yaml
import qualified Paths_planning_poker
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), stdout)
import qualified System.IO as IO

import qualified Logger
import qualified Core
import qualified Web

data Config = Config
  { cLogger :: Logger.Config
  , cWeb :: Web.Config
  }

instance Semigroup Config where
  (<>) l r =
    Config
      { cLogger = cLogger l <> cLogger r
      , cWeb = cWeb l <> cWeb r
      }

instance Monoid Config where
  mempty =
    Config
      { cLogger = mempty
      , cWeb = mempty
      }

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Main.Server.Config" $ \o ->
    Config
      <$> o A..:? "logger" A..!= mempty
      <*> o A..:? "web" A..!= mempty

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [] -> run "config.yaml"
    [configPath] -> run configPath
    _ -> do
      IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <conf>"
      exitFailure

run :: FilePath -> IO ()
run configPath = do
  IO.hSetBuffering stdout LineBuffering
  IO.hPutStrLn IO.stderr $
    "Booting planning-poker v" ++ showVersion Paths_planning_poker.version

  errOrConfig <- Yaml.decodeFileEither configPath
  Config{..} <- either (fail . show) return errOrConfig

  stateRef <- newTVarIO Core.initState

  Logger.withHandle cLogger $ \logger ->
    Web.withHandle cWeb logger stateRef $ \web ->
      Web.run web

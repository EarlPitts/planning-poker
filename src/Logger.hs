{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger (
  Handle (..),
  Config (..),
  withHandle,
  logDebug,
  logInfo,
  logWarn,
  logError,
) where

import Control.Applicative (Alternative (..))
import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Prelude hiding (error, log)

data Verbosity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Ord, Eq)

instance A.FromJSON Verbosity where
  parseJSON = A.withText "FromJSON Logger.Verbosity" $ \case
    "debug" -> pure Debug
    "info" -> pure Info
    "warning" -> pure Warning
    "error" -> pure Error
    t -> fail $ "Invalid verbosity: " <> T.unpack t

newtype Config = Config
  { cVerbosity :: Maybe Verbosity
  }
  deriving (Show)

instance Semigroup Config where
  Config v0 <> Config v1 = Config (v0 <|> v1)

instance Monoid Config where
  mempty = Config empty

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Logger.Config" $ \o ->
    Config <$> o A..:? "verbosity"

newtype Handle = Handle
  { hConfig :: Config
  }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle c = bracket (new c) close

new :: Config -> IO Handle
new = pure . Handle

close :: Handle -> IO ()
close = const $ pure ()

log :: Handle -> Verbosity -> String -> IO ()
log (Handle (Config cVerb)) v msg =
  when (verbosity <= v) $ putStrLn ("[" <> show v <> "] " <> msg)
 where
  verbosity = fromMaybe Debug cVerb

logDebug, logInfo, logWarn, logError :: Handle -> String -> IO ()
logDebug h = log h Debug
logInfo h = log h Info
logWarn h = log h Warning
logError h = log h Error

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Web (
  Handle (..),
  Config (..),
  withHandle,
  run,
) where

import Control.Applicative (empty, (<|>))
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Logger
import Lucid
import Network.HTTP.Types.Status (status404)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

data Config = Config
  { cPort :: Maybe Int
  , cDomain :: Maybe T.Text
  }
  deriving (Show)

instance Semigroup Config where
  (<>) c1 c2 =
    Config
      { cPort = cPort c1 <|> cPort c2
      , cDomain = cDomain c1 <|> cDomain c2
      }

instance Monoid Config where
  mempty = Config empty empty

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Web.Config" $ \o ->
    Config
      <$> o A..:? "port"
      <*> o A..:? "domain"

data Handle = Handle
  { hConfig :: Config
  , hLogger :: Logger.Handle
  }

withHandle ::
  Config ->
  Logger.Handle ->
  (Handle -> IO a) ->
  IO a
withHandle config logger f =
  f $ Handle config logger

run :: Handle -> IO ()
run h = Scotty.scotty port (app h)
 where
  port = fromMaybe 8000 $ cPort (hConfig h)

app :: Handle -> ScottyM ()
app h = do
  Scotty.get "/" $ do
    Scotty.html $ renderText "sajt"

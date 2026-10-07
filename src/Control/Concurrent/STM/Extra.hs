module Control.Concurrent.STM.Extra (
  transact,
) where

import Control.Concurrent.STM

transact :: TVar s -> (s -> Either e (s, a)) -> STM (Either e (s, a))
transact t f = do
  state <- readTVar t
  case f state of
    Left err -> pure $ Left err
    Right (newState, result) -> do
      writeTVar t newState
      pure $ Right (newState, result)

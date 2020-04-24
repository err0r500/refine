module Utils where

import           RIO
import           Test.Hspec

import           Lib
import qualified Adapter.InMemory.Logger       as Logger
import qualified Adapter.InMemory.NodeRepo     as InMem

-- creates an empty state
getEmptyState :: (MonadIO m) => m State
getEmptyState = do
  state  <- newTVarIO $ InMem.NodeStore mempty
  logger <- newTVarIO $ Logger.Logs []
  return (state, logger)


checkLogs :: (Show a) => State -> [a] -> IO ()
checkLogs state expectedLogs = do
  logs <- run state Logger.getLogs
  length logs `shouldBe` length expectedLogs
  logs `shouldMatchList` map tshow expectedLogs


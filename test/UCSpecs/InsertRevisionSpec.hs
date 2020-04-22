{-# LANGUAGE MonoLocalBinds #-}
module UCSpecs.InsertRevisionSpec
        ( spec
        )
where

import           ClassyPrelude
import           Test.Hspec

import           Lib
import qualified Domain.Revision               as D
import qualified Usecase.InsertRevision        as UC
import qualified Adapter.InMemory.Logger       as Logger
import qualified Adapter.InMemory.NodeRepo     as InMem

-- creates an empty state
getEmptyState :: (MonadIO m) => m State
getEmptyState = do
        state  <- newTVarIO $ InMem.NodeStore mempty
        logger <- newTVarIO $ Logger.Logs []
        return (state, logger)

-- apply the functions to get the usecase logic
ucLogic :: UC.InsertRevision InMemoryApp
ucLogic = UC.insertRevision InMem.getNodeContentByHash

-- get the IO usecase logic
insertRevision :: State -> UC.InsertRevision IO
insertRevision state hash edits = run state $ ucLogic hash edits

getLogs :: (Show a) => State -> [a] -> IO ()
getLogs state expectedLogs = do
        logs <- run state Logger.getLogs
        length logs `shouldBe` length expectedLogs
        logs `shouldMatchList` map tshow expectedLogs


-- SPECS
spec :: Spec
spec = do
        let     pHash = D.Hash "pHash"
                pNode = D.Node pHash "content"

        describe "success" -- success cases
                $ it "returns nothing if everything went well"
                $ do
                          state   <- getEmptyState
                          Nothing <- run state $ InMem.insertNode pNode
                          resp    <- insertRevision state pHash [D.Edit (1, 2) "edit"]
                          resp `shouldBe` Nothing

        describe "failure" -- failure cases
                $ it "returns parent not found"
                $ do
                          state <- getEmptyState
                          resp  <- insertRevision state pHash [D.Edit (1, 2) "edit"]
                          resp `shouldBe` Just UC.ParentNodeNotFound
                          getLogs state [D.ErrRevisionNotFound]


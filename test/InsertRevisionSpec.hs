{-# LANGUAGE MonoLocalBinds #-}
module InsertRevisionSpec
        ( spec
        )
where

import           ClassyPrelude
import           Test.Hspec

import qualified Adapter.InMemory.Logger       as Logger
import qualified Adapter.InMemory.NodeRepo     as InMem
import           App
import qualified Usecase.InsertRevision        as UC
import qualified Domain.Revision               as D

-- creates an empty state
getEmptyState :: (MonadIO m) => m App.State
getEmptyState = do
        state  <- newTVarIO $ InMem.NodeStore mempty
        logger <- newTVarIO $ Logger.Logs []
        return (state, logger)

-- apply the functions to get the usecase logic
ucLogic :: UC.InsertRevision InMemoryApp
ucLogic = UC.insertRevision InMem.getNodeContentByHash

-- get the IO usecase logic
insertRevision :: App.State -> UC.InsertRevision IO
insertRevision state hash edits = App.run state $ ucLogic hash edits

spec :: Spec
spec = do
        let parentNode = (D.Node (D.Hash "hash") "content")

        describe "success" $ it "returns nothing if everything went well" $ do
                state   <- getEmptyState
                Nothing <- App.run state $ InMem.insertNode parentNode
                resp    <- insertRevision state
                                          (D.Hash "hash")
                                          [D.Edit (1, 2) "edit"]
                resp `shouldBe` Nothing

        describe "failure" $ it "returns parent not found" $ do
                state <- getEmptyState
                resp  <- insertRevision state
                                        (D.Hash "hash")
                                        [D.Edit (1, 2) "edit"]
                resp `shouldBe` (Just UC.ParentNodeNotFound)


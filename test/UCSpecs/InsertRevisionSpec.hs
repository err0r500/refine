module UCSpecs.InsertRevisionSpec
  ( spec
  )
where

import           RIO
import           Test.Hspec

import           Lib
import           Utils
import qualified Domain.Node                   as D
import qualified Domain.Revision               as D
import qualified Usecase.InsertRevision        as UC
import qualified Adapter.InMemory.NodeRepo     as InMem

-- apply the functions to get the usecase logic
ucLogic :: UC.InsertRevision InMemoryApp
ucLogic = UC.insertRevision InMem.getNodeContentByHash

-- get the IO usecase logic
insertRevision :: State -> UC.InsertRevision IO
insertRevision state hash edits = run state $ ucLogic hash edits

-- SPECS
spec :: Spec
spec = do
  let pHash      = D.Hash "pHash"
      pNode      = D.Node pHash "content"
      validEdits = [D.Edit (1, 2) "edit", D.Edit (3, 4) "edit2"]

  describe "happy cases" -- success cases
    $ it "returns nothing"
    $ do
        state   <- getEmptyState
        Nothing <- run state $ InMem.insertNode pNode
        resp    <- insertRevision state pHash validEdits
        resp `shouldBe` Nothing

  describe "failure cases" $ do
    it "fails without parent" $ do
      state <- getEmptyState
      resp  <- insertRevision state pHash validEdits
      resp `shouldBe` Just UC.ParentNodeNotFound
      checkLogs state [D.ErrRevisionNotFound]

    it "fails with invalid edits" $ do
      state <- getEmptyState
      resp  <- insertRevision state pHash [D.Edit (2, 1) "edit"]
      resp `shouldBe` Just UC.InvalidRevision
      checkLogs state [D.ErrInvalidEdits]



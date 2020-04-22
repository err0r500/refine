module Usecase.InsertRevision where

import           ClassyPrelude           hiding ( log )
import qualified Usecase.Interactor            as UC
import qualified Domain.Message                as D
import qualified Domain.Revision               as D

data Err = InvalidRevision
  | ParentNodeNotFound
    deriving (Show, Eq)

type InsertRevision m = Monad m => D.Hash -> [D.Edit] -> m (Maybe Err)

insertRevision :: UC.Logger m => UC.GetNodeContentByHash m -> InsertRevision m
insertRevision getNodeContentByHash parent edits =
        let r = D.newRevision parent edits
        in  case r of
                    Left err -> do
                            UC.log [(D.InfoMsg err)]
                            pure $ Just InvalidRevision
                    Right c -> do
                            mayNC <- getNodeContentByHash parent
                            case mayNC of
                                    Left err -> do
                                            UC.log [(D.WarnMsg err)]
                                            pure $ Just ParentNodeNotFound
                                    Right _ -> pure $ Nothing


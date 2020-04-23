module Usecase.Interactor where

import           ClassyPrelude

import qualified Adapter.Logger                as Logger
import qualified Domain.Node                   as D
import qualified Domain.Revision               as D

data Interactor m = Interactor {
  nodeRepo_ :: Monad m => NodeRepo m,
  revisionRepo_ :: Monad m => RevisionRepo m
}

-- repos
data NodeRepo m = NodeRepo {
  insertNode_ :: Monad m => InsertNode m,
  getNodeContentByHash_ :: Monad m => GetNodeContentByHash m
}

data RevisionRepo m = RevisionRepo {
  insertRevision_ :: Monad m => InsertRevision m
}


-- types are there for usecases functions signatures
type InsertNode m = Monad m => D.Node -> m (Maybe D.RevError)
type GetNodeContentByHash m = Monad m => D.Hash -> m (Either D.RevError Text)

type InsertRevision m = Monad m => Text -> D.Revision -> m (Maybe D.RevError)

-- Logger
class Monad m =>
      Logger m
    where
    log :: Logger.Loggable a => [a] -> m ()


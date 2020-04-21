module Usecase.Interactor where

import           ClassyPrelude
import qualified Adapter.Logger                as Logger
import qualified Domain.Revision               as D

data Interactor m = Interactor {
  nodeRepo_ :: Monad m => NodeRepo m
}

data NodeRepo m = NodeRepo {
  insertNode_ :: Monad m => InsertNode m,
  getNodeContentByHash_ :: Monad m => GetNodeContentByHash m
}

-- types are there for usecases functions signatures
type InsertNode m = Monad m => D.Node -> m (Maybe D.RevError)
type GetNodeContentByHash m = Monad m => D.Hash -> m (Either D.RevError Text)

-- Logger
class Monad m =>
      Logger m
    where
    log :: Logger.Loggable a => [a] -> m ()

module Usecase.Interactor where

import           ClassyPrelude
import qualified Adapter.Logger                as Logger
import qualified Domain.Revision               as D

data Interactor m = Interactor {
  getNodeContentByHash_ :: Monad m => GetNodeContentByHash m
}

type GetNodeContentByHash m = Monad m => Text -> m (Either D.RevError Text)

-- Logger
class Monad m =>
      Logger m
    where
    log :: Logger.Loggable a => [a] -> m ()

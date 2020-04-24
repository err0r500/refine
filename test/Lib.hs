module Lib where

import           RIO

import qualified Adapter.InMemory.Logger       as InMem
import qualified Adapter.InMemory.NodeRepo     as InMem
import qualified Adapter.Logger                as Katip
import           Usecase.Interactor
import           Domain.Revision               as D


type State = (TVar InMem.NodeStore, TVar InMem.Logs)

newtype InMemoryApp a = InMemoryApp
    { unApp :: RIO State a
    } deriving (Functor, Applicative, Monad, MonadUnliftIO, MonadThrow, MonadReader State, MonadIO)

run :: State -> InMemoryApp a -> IO a
run state app = runRIO state (unApp app)

instance Logger InMemoryApp where
  log = InMem.log

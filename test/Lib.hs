module Lib where

import           RIO

import qualified Adapter.InMemory.Logger       as InMem
import qualified Adapter.InMemory.NodeRepo     as InMem
import qualified Adapter.Logger                as Katip
import           Usecase.Interactor
import           Domain.Revision               as D


type State = (TVar InMem.NodeStore, TVar InMem.Logs)

newtype App a = App ( RIO State a )
  deriving (Functor, Applicative, Monad, MonadUnliftIO, MonadThrow, MonadReader State, MonadIO)

run :: State -> App a -> IO a
run state (App app) = runRIO state app

instance Logger App where
  log = InMem.log

module Lib where

import qualified Adapter.InMemory.Logger       as InMem
import qualified Adapter.InMemory.NodeRepo     as InMem
import           ClassyPrelude
import qualified Adapter.Logger                as Katip
import           Usecase.Interactor

type State = (TVar InMem.NodeStore, TVar InMem.Logs)

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT State IO a
    } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO)

run :: State -> InMemoryApp a -> IO a
run state app = runReaderT (unApp app) state

instance Logger InMemoryApp where
        log = InMem.log

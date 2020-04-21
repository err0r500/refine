module Lib (start) where

import           ClassyPrelude
import qualified Network.Wai.Handler.Warp      as Warp

import qualified Config.Config                 as Config
import qualified Adapter.Http.Router           as HttpRouter
import qualified Adapter.Logger                as Logger
import           Domain.Revision
import qualified Adapter.InMemory.RevRepo      as InMem
import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC
import qualified Usecase.InsertRevision        as UC

type RevisionStore = TVar InMem.RevisionStore

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT RevisionStore IO a
    } deriving (Applicative, Functor, Monad, MonadReader RevisionStore, MonadIO)

run :: RevisionStore -> InMemoryApp a -> IO a
run state app = runReaderT (unApp app) state

getFreshState :: (MonadIO m) => m RevisionStore
getFreshState = newTVarIO $ InMem.RevisionStore mempty

start :: IO ()
start = do
        putStrLn "== Refine =="
        state  <- getFreshState
        router <- HttpRouter.start (logicHandler interactor) $ run state
        port   <- Config.getIntFromEnv "PORT" 3000
        putStrLn $ "starting server on port: " ++ tshow port
        Warp.run port router

interactor :: UC.Interactor InMemoryApp
interactor =
        UC.Interactor { UC.getNodeContentByHash_ = InMem.getRevContentByHash }

logicHandler :: UC.Interactor InMemoryApp -> UC.LogicHandler InMemoryApp
logicHandler i = UC.LogicHandler
        (UC.insertRev $ UC.getNodeContentByHash_ i)


instance UC.Logger InMemoryApp where
        log = Logger.log


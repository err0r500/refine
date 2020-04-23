module Lib
        ( start
        )
where

import           ClassyPrelude
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Control.Monad.Catch           as Catch

import qualified Adapter.Http.Router           as HttpRouter
import qualified Adapter.InMemory.NodeRepo     as InMem
import qualified Adapter.InMemory.RevisionRepo as InMem
import qualified Adapter.Logger                as Logger
import qualified Config.Config                 as Config
import qualified Usecase.InsertRevision        as UC
import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC

start :: IO ()
start = do
        putStrLn "== Refine =="
        state  <- freshState
        router <- HttpRouter.start (logicHandler interactor) $ run state
        port   <- Config.getIntFromEnv "PORT" 3000
        putStrLn $ "starting server on port: " ++ tshow port
        Warp.run port router


type State = (TVar InMem.NodeStore, TVar InMem.RevisionStore)

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT State IO a
    } deriving (Functor, Applicative, Monad, Catch.MonadThrow, Catch.MonadCatch, MonadReader State, MonadIO)


run :: State -> InMemoryApp a -> IO a
run state app = runReaderT (unApp app) state

freshState :: (MonadIO m) => m State
freshState = do
        nodes     <- newTVarIO $ InMem.NodeStore mempty
        revisions <- newTVarIO $ InMem.RevisionStore mempty
        return (nodes, revisions)

interactor :: UC.Interactor InMemoryApp
interactor = UC.Interactor { UC.nodeRepo_ = nodeRepo, UC.revisionRepo_ = revisionRepo }
    where
        nodeRepo     = UC.NodeRepo InMem.insertNode InMem.getNodeContentByHash
        revisionRepo = UC.RevisionRepo InMem.insertRevision

logicHandler :: UC.Interactor InMemoryApp -> UC.LogicHandler InMemoryApp
logicHandler i =
        UC.LogicHandler -- the usecases :
                        (UC.insertRevision (UC.getNodeContentByHash_ $ UC.nodeRepo_ i))


instance UC.Logger InMemoryApp where
        log = Logger.log


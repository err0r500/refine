module Lib
        ( start
        )
where

import           ClassyPrelude
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Control.Monad.Catch           as Catch

import qualified Adapter.Http.Router           as HttpRouter
import qualified Adapter.InMemory.NodeRepo     as InMem
import qualified Adapter.Logger                as Logger
import qualified Config.Config                 as Config
import qualified Usecase.InsertRevision        as UC
import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC


type NodeStore = TVar InMem.NodeStore

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT NodeStore IO a
    } deriving (Applicative, Functor, Monad, Catch.MonadThrow, Catch.MonadCatch, MonadReader NodeStore, MonadIO)

run :: NodeStore -> InMemoryApp a -> IO a
run state app = runReaderT (unApp app) state

getFreshState :: (MonadIO m) => m NodeStore
getFreshState = newTVarIO $ InMem.NodeStore mempty

start :: IO ()
start = do
        putStrLn "== Refine =="
        state  <- getFreshState
        router <- HttpRouter.start (logicHandler interactor) $ run state
        port   <- Config.getIntFromEnv "PORT" 3000
        putStrLn $ "starting server on port: " ++ tshow port
        Warp.run port router

interactor :: UC.Interactor InMemoryApp
interactor = UC.Interactor { UC.nodeRepo_ = nodeRepo }
        where nodeRepo = UC.NodeRepo InMem.insertNode InMem.getNodeContentByHash

logicHandler :: UC.Interactor InMemoryApp -> UC.LogicHandler InMemoryApp
logicHandler i =
        UC.LogicHandler -- the usecases :
                        (UC.insertRevision (UC.getNodeContentByHash_ $ UC.nodeRepo_ i))


instance UC.Logger InMemoryApp where
        log = Logger.log


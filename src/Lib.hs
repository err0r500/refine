{-# LANGUAGE TypeSynonymInstances #-}
module Lib
  ( start
  )
where

import           RIO
import           System.IO

import qualified Network.Wai.Handler.Warp      as Warp

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
  putStrLn $ "starting server on port: " ++ show port
  Warp.run port router


type State = (TVar InMem.NodeStore, TVar InMem.RevisionStore)

newtype App a = App ( RIO State a )
  deriving (Functor, Applicative, Monad, MonadUnliftIO, MonadThrow, MonadReader State, MonadIO)

run :: State -> App a -> IO a
run state (App app) = runRIO state app

freshState :: (MonadIO m) => m State
freshState = do
  nodes     <- newTVarIO $ InMem.NodeStore mempty
  revisions <- newTVarIO $ InMem.RevisionStore mempty
  return (nodes, revisions)

interactor :: UC.Interactor App
interactor = UC.Interactor { UC.nodeRepo_ = nodeRepo, UC.revisionRepo_ = revisionRepo }
 where
  nodeRepo     = UC.NodeRepo InMem.insertNode InMem.getNodeContentByHash
  revisionRepo = UC.RevisionRepo InMem.insertRevision

logicHandler :: UC.Interactor App -> UC.LogicHandler App
logicHandler i =
  -- the usecases :
  UC.LogicHandler (UC.insertRevision (UC.getNodeContentByHash_ $ UC.nodeRepo_ i))

instance UC.Logger App where
  log = Logger.log


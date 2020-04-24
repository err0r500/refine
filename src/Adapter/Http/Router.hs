module Adapter.Http.Router where

import           ClassyPrelude
import qualified Network.HTTP.Types            as HttpTypes
                                                ( status200
                                                , status404
                                                )
import qualified Network.Wai                   as Wai
                                                ( Application
                                                , Response
                                                )
import qualified Web.Scotty.Trans              as ScottyTrans

import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC
import qualified Adapter.Http.InsertRevision   as HttpAdapter

start
  :: (MonadIO m, UC.Logger m)
  => UC.LogicHandler m
  -> (m Wai.Response -> IO Wai.Response)
  -> IO Wai.Application
start logicHandler runner = ScottyTrans.scottyAppT
  runner
  (do
    ScottyTrans.get "/" $ ScottyTrans.status HttpTypes.status200 -- health check
    ScottyTrans.put "/rev/:name" $ HttpAdapter.insertRevision $ UC.doInsertRevision logicHandler
    ScottyTrans.notFound $ ScottyTrans.status HttpTypes.status404
  )


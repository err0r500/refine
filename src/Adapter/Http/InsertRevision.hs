module Adapter.Http.InsertRevision where

import           ClassyPrelude
import           Network.HTTP.Types             ( status200
                                                , status400
                                                )

import qualified Web.Scotty.Trans              as ScottyT
import           Usecase.InsertRevision        as UC
                                                ( InsertRevision )

insertRevisionHandler :: Monad m => UC.InsertRevision m -> ScottyT.ActionT LText m ()
insertRevisionHandler insertRevision = do
        name <- ScottyT.param "name"
        resp <- lift $ insertRevision name
        case resp of
                Just _  -> ScottyT.status status400
                Nothing -> ScottyT.status status200

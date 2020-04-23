module Adapter.Http.InsertRevision where

import           ClassyPrelude
import qualified Network.HTTP.Types            as HttpTypes
                                                ( status501 )
import qualified Web.Scotty.Trans              as ScottyT

import           Usecase.InsertRevision        as UC

insertRevision :: Monad m => UC.InsertRevision m -> ScottyT.ActionT LText m ()
insertRevision insertRevision = do
        ScottyT.status HttpTypes.status501
  {-
        name   <- ScottyT.param "parenthash"
        change <- ScottyT.param "change"
        resp   <- lift $ insertRevision name change
        case resp of
                Just _  -> ScottyT.status status400
                Nothing -> ScottyT.status status200

-}

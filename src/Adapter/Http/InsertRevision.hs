module Adapter.Http.InsertRevision where

import           ClassyPrelude
import           Network.HTTP.Types             ( status501 )

import qualified Web.Scotty.Trans              as ScottyT
import           Usecase.InsertRevision        as UC
                                                ( InsertRevision )

insertRevisionHandler
        :: Monad m
        => UC.InsertRevision m
        -> ScottyT.ActionT LText m ()
insertRevisionHandler insertRevision = do
        ScottyT.status status501
  {-
        name   <- ScottyT.param "parenthash"
        change <- ScottyT.param "change"
        resp   <- lift $ insertRevision name change
        case resp of
                Just _  -> ScottyT.status status400
                Nothing -> ScottyT.status status200

-}

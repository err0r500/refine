module Usecase.InsertRevision where

import           ClassyPrelude           hiding ( log )
import qualified Usecase.Interactor            as UC
import qualified Domain.Message                as D
import qualified Domain.Revision               as D
import qualified Control.Monad.Catch           as C

data Err = InvalidRevision
  | ParentNodeNotFound
    deriving (Show, Eq)


type InsertRevision m = Monad m => D.Hash -> [D.Edit] -> m (Maybe Err)

insertRevision :: (UC.Logger m, C.MonadCatch m, Exception D.RevError) => UC.GetNodeContentByHash m -> InsertRevision m
insertRevision getNodeContentByHash parent edits = C.catch
        (do
                _ <- uc getNodeContentByHash parent edits
                pure Nothing
        )
        (\e -> do
                err <- handleExceptions e
                pure $ Just err
        )


uc :: (C.MonadThrow m, Exception D.RevError) => UC.GetNodeContentByHash m -> D.Hash -> [D.Edit] -> m ()
uc getNodeContentByHash parent edits = do
        revision         <- newRevision parent edits
        getParentContent <- getParentContent getNodeContentByHash parent
        pure ()

newRevision :: (C.MonadThrow m, Exception D.RevError) => D.Hash -> [D.Edit] -> m D.Revision
newRevision parent edits = case D.newRevision parent edits of
        Left  e        -> C.throwM e
        Right revision -> pure revision

getParentContent :: (C.MonadThrow m, Exception D.RevError) => UC.GetNodeContentByHash m -> D.Hash -> m Text
getParentContent getNodeContentByHash parent = do
        mayNC <- getNodeContentByHash parent
        case mayNC of
                Right may -> pure may
                Left  e   -> C.throwM e

handleExceptions :: (UC.Logger m, C.MonadCatch m, Exception D.RevError) => D.RevError -> m Err
handleExceptions e = case e of
        D.ErrRevisionNotFound -> do
                UC.log [D.WarnMsg e]
                pure ParentNodeNotFound
        D.ErrInvalidEdits -> do
                UC.log [D.InfoMsg e]
                pure InvalidRevision

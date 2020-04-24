module Usecase.InsertRevision
  ( insertRevision
  , InsertRevision
  , Err(..)
  )
where

import RIO
import qualified Usecase.Interactor            as UC
import qualified Domain.Message                as D
import qualified Domain.Node                   as D
import qualified Domain.Revision               as D

-- public --
data Err = InvalidRevision
  | ParentNodeNotFound
    deriving (Show, Eq)

type InsertRevision m = Monad m => D.Hash -> [D.Edit] -> m (Maybe Err)

insertRevision
  :: (UC.Logger m, MonadThrow m, MonadUnliftIO m, Exception D.RevError)
  => UC.GetNodeContentByHash m
  -> InsertRevision m
insertRevision getNodeContentByHash parentHash edits = catch
  (do
    uc getNodeContentByHash parentHash edits
    pure Nothing
  )
  (\e -> do
    err <- handleExceptions e
    pure $ Just err
  )



-- private --
uc
  :: (MonadThrow m, Exception D.RevError)
  => UC.GetNodeContentByHash m
  -> D.Hash
  -> [D.Edit]
  -> m ()
uc getNodeContentByHash parentHash edits = do
  revision      <- newRevision parentHash edits
  parentContent <- getParentContent getNodeContentByHash parentHash
  pure ()

newRevision :: (MonadThrow m, Exception D.RevError) => D.Hash -> [D.Edit] -> m D.Revision
newRevision parentHash edits = case D.newRevision parentHash edits of
  Right revision -> pure revision
  Left  e        -> throwM e

getParentContent
  :: (MonadThrow m, Exception D.RevError) => UC.GetNodeContentByHash m -> D.Hash -> m Text
getParentContent getNodeContentByHash parentHash = do
  mayNC <- getNodeContentByHash parentHash
  case mayNC of
    Right content -> pure content
    Left  e       -> throwM e

handleExceptions :: (UC.Logger m, MonadUnliftIO m, Exception D.RevError) => D.RevError -> m Err
handleExceptions e = case e of
  D.ErrRevisionNotFound -> do
    UC.log [D.WarnMsg e]
    pure ParentNodeNotFound
  D.ErrInvalidEdits -> do
    UC.log [D.InfoMsg e]
    pure InvalidRevision

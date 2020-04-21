module Adapter.InMemory.RevRepo where

import           ClassyPrelude
import qualified Data.Has                      as DH
import qualified Domain.Revision               as D

newtype RevisionStore = RevisionStore
    { revisions :: Map Text D.Node
    }

-- the in-memory data store
type InMemory r m = (DH.Has (TVar RevisionStore) r, MonadReader r m, MonadIO m)


getNodeContentByHash :: InMemory r m => D.Hash -> m (Either D.RevError Text)
getNodeContentByHash h = do
        mayNode <- nodeSearch (\n -> (D.contentHash n) == h)
        case mayNode of
                Just found -> pure $ Right (D.content found)
                Nothing    -> pure $ Left D.ErrRevisionNotFound


nodeSearch :: InMemory r m => (D.Node -> Bool) -> m (Maybe D.Node)
nodeSearch filter_ = do
        tvar <- asks DH.getter
        atomically $ do
                state <- readTVar tvar
                case filter filter_ $ toList (revisions state) of
                        []      -> pure Nothing
                        (x : _) -> pure (Just x)



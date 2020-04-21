module Adapter.InMemory.RevRepo where

import           ClassyPrelude
import qualified Data.Has                      as DH
import qualified Domain.Revision               as D

newtype RevisionStore = RevisionStore
    { users :: Map Text D.Revision
    }

-- the in-memory data store
type InMemory r m = (DH.Has (TVar RevisionStore) r, MonadReader r m, MonadIO m)


getRevContentByHash :: InMemory r m => Text -> m (Either D.RevError Text)
getRevContentByHash _ = do
        pure $ Right "salut"

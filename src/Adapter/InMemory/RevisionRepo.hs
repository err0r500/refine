module Adapter.InMemory.RevisionRepo where

import           RIO
import           RIO.Map                       as Map
import qualified Data.Has                      as Has
import qualified Domain.Revision               as D

newtype RevisionStore = RevisionStore
    { revisions :: Map Text D.Revision
    }

-- in-memory data store
type InMemory r m = (Has.Has (TVar RevisionStore) r, MonadReader r m, MonadIO m)

insertRevision :: InMemory r m => Text -> D.Revision -> m (Maybe D.RevError)
insertRevision id revision = do
  tvar <- asks Has.getter
  atomically $ do
    state <- readTVar tvar
    writeTVar tvar state { revisions = Map.insert id revision $ revisions state }
    pure Nothing


module Adapter.InMemory.NodeRepo where

import           ClassyPrelude
import qualified Data.Has                      as DH
import qualified Domain.Revision               as D

newtype NodeStore = NodeStore
    { nodes :: Map D.Hash D.Node
    }

-- the in-memory data store
type InMemory r m = (DH.Has (TVar NodeStore) r, MonadReader r m, MonadIO m)

insertNode :: InMemory r m => D.Node -> m (Maybe D.RevError)
insertNode node = do
        tvar <- asks DH.getter
        atomically $ do
                state <- readTVar tvar
                writeTVar tvar state { nodes = insertMap (D.contentHash node) node $ nodes state }
                pure $ Nothing


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
                case filter filter_ $ toList (nodes state) of
                        []      -> pure Nothing
                        (x : _) -> pure (Just x)



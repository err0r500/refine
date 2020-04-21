module Usecase.InsertRevision where

import           ClassyPrelude
import qualified Usecase.Interactor            as UC
import qualified Domain.Revision               as D

type InsertRevision m = Monad m => Text -> m (Maybe D.RevError)

-- TODO : fill the usecase
insertRev
        :: UC.Logger m
        => UC.GetNodeContentByHash m
        -> Text
        -> m (Maybe D.RevError)
insertRev getNodeContentByHash parentHash = do
        mayNodeContent <- getNodeContentByHash parentHash
        case mayNodeContent of
          Left err -> pure $ Just err
          Right _ -> pure $ Nothing

--someFunc :: IO ()
--someFunc =
--  -- example of new change added to parent
--        let
--                n = newRevision
--                        "parent"
--                        [Edit (0, 12) "my edit 1", Edit (13, 14) "my edit 2"]
--        in  case n of
--                    Left  err -> putStrLn $ pack err
--                    Right c   -> putStrLn $ pack $ show n



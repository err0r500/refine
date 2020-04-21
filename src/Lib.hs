module Lib
        ( someFunc
        )
where

import           ClassyPrelude
import           Domain.Change


someFunc :: IO ()
someFunc =
  -- example of new change added to parent
        let
                n = newRevision
                        "parent"
                        [Edit (0, 12) "my edit 1", Edit (13, 14) "my edit 2"]
        in  case n of
                    Left  err -> putStrLn $ pack err
                    Right c   -> putStrLn $ pack $ show n


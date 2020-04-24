module Config.Config
  ( getIntFromEnv
  )
where

import           ClassyPrelude
import qualified System.Environment            as Environment

getIntFromEnv :: String -> Int -> IO Int
getIntFromEnv key defaultValue = do
  result <- tryIOError $ Environment.getEnv key
  case result of
    Left  _           -> pure defaultValue
    Right portFromEnv -> case readMay portFromEnv :: Maybe Int of
      Just x  -> pure x
      Nothing -> pure defaultValue


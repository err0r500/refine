{-# LANGUAGE GADTs #-}

module Domain.Message where

import           ClassyPrelude

data Message a where
  ErrMsg ::Show a => a -> Message a
  WarnMsg ::Show a => a -> Message a
  InfoMsg ::Show a => a -> Message a
  DebugMsg ::Show a => a -> Message a


instance Show (Message a) where
  show (ErrMsg   a) = show a
  show (WarnMsg  a) = show a
  show (InfoMsg  a) = show a
  show (DebugMsg a) = show a

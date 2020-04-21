module Usecase.LogicHandler where

import qualified Usecase.InsertRevision      as UC

data LogicHandler m = LogicHandler {
  insertRevision_ :: UC.InsertRevision m
}

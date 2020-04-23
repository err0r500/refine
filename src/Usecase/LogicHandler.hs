module Usecase.LogicHandler where

import qualified Usecase.InsertRevision      as UC

data LogicHandler m = LogicHandler {
  doInsertRevision :: UC.InsertRevision m
}

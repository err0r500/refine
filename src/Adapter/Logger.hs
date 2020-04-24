{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Adapter.Logger where

import           RIO
import qualified Control.Exception             as Exception
import qualified Katip                         as K

import qualified Domain.Message                as D
import qualified Domain.Revision               as D

class Show a => Loggable a where
    type F a
    log' :: a -> F (IO ())
    show' :: a -> Text

instance Show a => Loggable a where
  type F a = K.KatipContextT IO ()
  log' mess = $(K.logTM) K.ErrorS (K.showLS mess)
  show' = tshow

instance Loggable D.RevError where
  type F D.RevError = K.KatipContextT IO ()
  log' err = $(K.logTM) K.ErrorS (K.showLS err)
  show' = tshow

instance Show a => Loggable ( D.Message a ) where
  type F (D.Message a) = K.KatipContextT IO ()
  log' (D.ErrMsg   mess) = $(K.logTM) K.ErrorS (K.showLS $ show mess)
  log' (D.WarnMsg  mess) = $(K.logTM) K.WarningS (K.showLS $ show mess)
  log' (D.InfoMsg  mess) = $(K.logTM) K.InfoS (K.showLS $ show mess)
  log' (D.DebugMsg mess) = $(K.logTM) K.DebugS (K.showLS $ show mess)
  show' = tshow

log :: (MonadIO m, Loggable a) => [a] -> m ()
log elemsToLog = do
  handleScribe <- liftIO $ K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings
        =<< K.initLogEnv "refine" "prod"
  liftIO $ Exception.bracket mkLogEnv K.closeScribes $ \le ->
    K.runKatipContextT le () mempty $ mapM_ log' elemsToLog

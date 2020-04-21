{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Adapter.Logger where

import           Control.Exception
import           Katip
import           ClassyPrelude
import qualified Domain.Message                as D
import qualified Domain.Revision               as D

class Show a => Loggable a where
    type F a
    log' :: a -> F (IO ())
    show' :: a -> Text

instance Show a => Loggable a where
        type F a = KatipContextT IO ()
        log' mess = $(logTM) ErrorS (showLS mess)
        show' = tshow

instance Loggable D.RevError where
        type F D.RevError = KatipContextT IO ()
        log' err = $(logTM) ErrorS (showLS err)
        show' = tshow

instance Show a => Loggable ( D.Message a ) where
        type F (D.Message a) = KatipContextT IO ()
        log' (D.ErrMsg   mess) = $(logTM) ErrorS (showLS $ show mess)
        log' (D.WarnMsg mess) = $(logTM) WarningS (showLS $ show mess)
        log' (D.InfoMsg    mess) = $(logTM) InfoS (showLS $ show mess)
        log' (D.DebugMsg   mess) = $(logTM) DebugS (showLS $ show mess)
        show' = tshow

log :: (MonadIO m, Loggable a) => [a] -> m ()
log elemsToLog = do
        handleScribe <- liftIO
                $ mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
        let mkLogEnv =
                    registerScribe "stdout" handleScribe defaultScribeSettings
                            =<< initLogEnv "refine" "prod"
        liftIO $ Control.Exception.bracket mkLogEnv closeScribes $ \le ->
                runKatipContextT le () mempty $ mapM_ log' elemsToLog

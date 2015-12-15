{-# LANGUAGE OverloadedStrings #-}

module System.Log.MonadLogger.Syslog
       ( runSyslogLoggingT
       , syslogOutput
       , defaultSyslogOutput
       , formattedSyslogOutput
       )
       where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (unpack)
import System.Log.FastLogger (fromLogStr)
import System.Posix.Syslog (Priority (..), SyslogFn)

import Control.Monad.Logger

runSyslogLoggingT :: MonadIO m => SyslogFn -> LoggingT m a -> m a
runSyslogLoggingT syslog m = runLoggingT m (syslogOutput syslog)

syslogOutput :: SyslogFn -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
syslogOutput = defaultSyslogOutput

defaultSyslogOutput :: SyslogFn -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
defaultSyslogOutput syslog = formattedSyslogOutput syslog defaultLogStr

formattedSyslogOutput
  :: SyslogFn
  -> (Loc -> LogSource -> LogLevel -> LogStr -> LogStr)
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
formattedSyslogOutput syslog f l s level msg =
    syslog [levelToPriority level] (fromLogStr $ f l s level msg)

levelToPriority :: LogLevel -> Priority
levelToPriority LevelDebug = Debug
levelToPriority LevelInfo  = Info
levelToPriority LevelWarn  = Warning
levelToPriority LevelError = Error
levelToPriority (LevelOther level) =
    case level of
        "Emergency" -> Emergency
        "Alert"     -> Alert
        "Critical"  -> Critical
        "Notice"    -> Notice
        _ -> error $ "unknown log level: " ++ unpack level

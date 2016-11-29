{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Log.MonadLogger.Syslog
       ( runSyslogLoggingT
       , syslogOutput
       , defaultSyslogOutput
       , formattedSyslogOutput
       )
       where

import Control.Monad.Logger
import System.Posix.Syslog
import Data.Text (unpack)
import System.Log.FastLogger (fromLogStr)
#if MIN_VERSION_hsyslog(4,0,0)
#else
import qualified Data.ByteString.Char8 as BS8
#endif

-- | Runs a 'LoggingT', sending its output to the syslog.  The logs
-- are formatted the same as 'runStdoutLoggingT', but the 'LogLevel'
-- is converted to a syslog priority value (but still included in the
-- log message).
runSyslogLoggingT :: LoggingT m a -> m a
runSyslogLoggingT = (`runLoggingT` syslogOutput)

-- TODO: useSyslog allows giving a source name and should be more
-- efficient But it assumes IO.  Perhaps MonadBaseControl should be
-- used to generalize it.
--
-- Note that this probably shouldn't be the default implementation,
-- because these settings are process-wide:
-- https://hackage.haskell.org/package/hsyslog-2.0/docs/System-Posix-Syslog.html#v:withSyslog
--
-- So, concurrent use of this would step on eachother.
{-
runSyslogLoggingT :: MonadIO m => String -> LoggingT m a -> m a
runSyslogLoggingT source action =
  useSyslog source (runLoggingT action defaultSyslogOutput)
-}

-- | Same as 'defaultSyslogOutput'.
syslogOutput :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
syslogOutput = defaultSyslogOutput

-- | This invokes 'formattedSyslogOutput' with 'defaultLogStr'.  This
-- means that the resulting log messages are the same as the default
-- format used by "Control.Monad.Logger".
defaultSyslogOutput :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
defaultSyslogOutput = formattedSyslogOutput defaultLogStr

-- | Given a "Control.Monad.Logger" log formatter, this writes the log
-- to the syslog,
formattedSyslogOutput :: (Loc -> LogSource -> LogLevel -> LogStr -> LogStr)
                      -> Loc
                      -> LogSource
                      -> LogLevel
                      -> LogStr
                      -> IO ()
formattedSyslogOutput f l s level msg =
#if MIN_VERSION_hsyslog(4,0,0)
    withSyslog defaultConfig $ \syslog ->
        syslog USER
            (levelToPriority level)
            (fromLogStr $ f l s level msg)
#else
    syslog
        (levelToPriority level)
        (BS8.unpack $ fromLogStr $ f l s level msg)
#endif

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

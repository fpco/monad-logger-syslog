# Monad Logger for Syslog

![TravisCI](https://travis-ci.org/fpco/monad-logger-syslog.svg)
![Hackage](https://img.shields.io/hackage/v/monad-logger-syslog.svg)

## Install

    cabal install monad-logger-syslog

## Usage (example)

    import Control.Monad.Logger (logDebugN)
    import Control.Monad.Logger.Syslog (runSyslogLoggingT)
    import System.Posix.Syslog (withSyslog, defaultConfig)

    main :: IO ()
    main =
        withSyslog defaultConfig $ \syslog ->
          runSyslogLoggingT syslog (logDebugN "HELLO!")

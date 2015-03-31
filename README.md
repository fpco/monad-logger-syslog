# Monad Logger for Syslog

![img](//travis-ci.org/fpco/monad-logger-syslog.svg)
![img](//img.shields.io/hackage/v/monad-logger-syslog.svg)

## Install

    cabal install monad-logger-syslog

## Usage (example)

    import Control.Monad.Logger ( logDebugN  )
    import Control.Monad.Logger.Syslog ( runSyslogLoggingT )

    main :: IO ()
    main = runSyslogLoggingT (logDebugN "HELLO!")

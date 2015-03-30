# Install

    cabal install monad-logger-syslog

# Usage (example)

    import Control.Monad.Logger ( logDebugN  )
    import Control.Monad.Logger.Syslog ( runSyslogLoggingT )
    
    main :: IO ()
    main = runSyslogLoggingT (logDebugN "HELLO!")

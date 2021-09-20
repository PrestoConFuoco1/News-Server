module Logger (
    Handle(..),
    Priority (..),
    logDebug,
    logInfo,
    logWarning,
    logError,
    logFatal,
    LoggerEntry,
    simpleLog
) where

import Prelude hiding (log)
import qualified Data.Text as T (Text, pack, unpack)

newtype Handle = Handle { log :: Priority -> T.Text -> IO () }

type LoggerEntry = (Priority, T.Text)

data Priority = Debug
              | Info
              | Warning
              | Error
              | Fatal
            deriving (Eq, Ord, Show)


logDebug, logInfo, logWarning, logError :: Handle -> T.Text -> IO ()
logDebug = (`log` Debug)
logInfo = (`log` Info)
logWarning = (`log` Warning)
logError = (`log` Error)
logFatal = (`log` Fatal)

simpleLog = Handle $ \p s -> putStrLn $ '[' : show p ++ "]: " ++ T.unpack s

module App.Logger where

import Prelude hiding (log)
import qualified Data.Text as T (Text, pack, unpack)



newtype Handle m = Handle { log :: Priority -> T.Text -> m () }

type LoggerEntry = (Priority, T.Text)

data Priority = Debug
              | Info
              | Warning
              | Error
              | Fatal
            deriving (Eq, Ord, Show)


logDebug, logInfo, logWarning, logError :: Handle m -> T.Text -> m ()
logDebug = (`log` Debug)
logInfo = (`log` Info)
logWarning = (`log` Warning)
logError = (`log` Error)
logFatal = (`log` Fatal)

simpleLog :: Handle IO
simpleLog = Handle $ \p s -> putStrLn $ '[' : show p ++ "]: " ++ T.unpack s



module App.Logger where

import Prelude hiding (log)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T (hPutStrLn)
import Data.IORef

import qualified GHC.IO.Handle.Lock as Lk
import qualified System.Exit as Q (ExitCode (..), exitWith)
import qualified System.IO.Error as IOE
import qualified System.IO as S
import qualified Control.Monad.Catch as C
import Control.Monad (when)
import qualified Utils as S

newtype Handle m = Handle { log :: Priority -> T.Text -> m () }

type LoggerEntry = (Priority, T.Text)

data Priority = Debug
              | Info
              | Warning
              | Error
              | Fatal
            deriving (Eq, Ord, Show, Read)


logDebug, logInfo, logWarning, logError, logFatal :: Handle m -> T.Text -> m ()
logDebug = (`log` Debug)
logInfo = (`log` Info)
logWarning = (`log` Warning)
logError = (`log` Error)
logFatal = (`log` Fatal)

fileHandleToLogger :: S.Handle -> Handle IO
fileHandleToLogger h = Handle $ fileLogger h

simpleLog :: Handle IO
--simpleLog = Handle $ \p s -> S.hPutStrLn S.stderr $ '[' : show p ++ "]: " ++ T.unpack s
simpleLog = Handle $ fileLogger S.stderr

stdHandle :: Handle IO
stdHandle = Handle $ fileLogger S.stderr

fileLogger :: S.Handle -> Priority -> T.Text -> IO ()
fileLogger _ p s = S.hPutStrLn S.stderr $ '[' : show p ++ "]: " ++ T.unpack s

emptyLogger :: Handle IO
emptyLogger = Handle $ \_ _ -> return ()


logString :: Priority -> T.Text -> T.Text
logString pri s = "[" <> S.showText pri <> "]: " <> s



data LoggerConfig = LoggerConfig {
    lcFilter :: Priority -> Bool
    , lcPath :: FilePath
    }

data LoggerResources = LoggerResources {
    flHandle :: S.Handle
    }

pathToHandle :: FilePath -> IO S.Handle
pathToHandle path = do
    h <- S.openFile path S.AppendMode
    return h

initializeErrorHandler :: IOE.IOError -> IO a
initializeErrorHandler e = do
    logFatal stdHandle $ "failed to initialize logger"
    func e
    Q.exitWith (Q.ExitFailure 1)
  where
   func x
    | IOE.isAlreadyInUseError x = logError stdHandle lockedmsg
    | IOE.isPermissionError x   = logError stdHandle "not enough permissions"
    | otherwise = logError stdHandle $ "unexpected IO error: " <> T.pack (C.displayException x)

lockedmsg :: T.Text
lockedmsg = "target log file is locked"

initializeDefaultHandler :: C.SomeException -> IO a
initializeDefaultHandler e = do
    logFatal stdHandle $ "failed to initialize logger"
    logFatal stdHandle $ T.pack $ C.displayException e
    Q.exitWith (Q.ExitFailure 1)

withSelfSufficientLogger :: LoggerConfig -> (Handle IO -> IO a) -> IO a
withSelfSufficientLogger conf action = do
    C.bracket
        (initializeSelfSufficientLoggerResources conf)
        closeSelfSufficientLogger
        (\resourcesRef -> action $
            Handle $ selfSufficientLogger resourcesRef $
                lcFilter conf)

initializeSelfSufficientLoggerResources :: LoggerConfig -> IO (IORef LoggerResources)
initializeSelfSufficientLoggerResources conf = do
    h <- pathToHandle (lcPath conf) `C.catches`
        [C.Handler initializeErrorHandler,
         C.Handler initializeDefaultHandler]
    lockAcquired <- Lk.hTryLock h Lk.ExclusiveLock
    when (not lockAcquired) $ do
        logFatal stdHandle $ "failed to initialize logger"
        logFatal stdHandle lockedmsg
        Q.exitWith (Q.ExitFailure 1)
        
    let resources = LoggerResources {
            flHandle = h
            }
    resourcesRef <- newIORef resources
    return resourcesRef

closeSelfSufficientLogger :: IORef LoggerResources -> IO ()
closeSelfSufficientLogger resourcesRef = do
    resources <- readIORef resourcesRef
    let h = flHandle resources
    Lk.hUnlock h
    S.hClose h


selfSufficientLogger :: IORef LoggerResources -> (Priority -> Bool) -> Priority -> T.Text -> IO ()
selfSufficientLogger resourcesRef predicate pri s = do
    resources <- readIORef resourcesRef
    let action = when (predicate pri) $
                T.hPutStrLn (flHandle resources) (logString pri s)
                 >> T.hPutStrLn S.stderr (logString pri s)
        errHandler = \e -> loggerHandler resources e >>= writeIORef resourcesRef
    action `C.catch` errHandler

loggerHandler :: LoggerResources -> C.SomeException -> IO LoggerResources
loggerHandler resources e = do
    logError stdHandle "failed to use log file, error is:"
    logError stdHandle $ T.pack $ C.displayException e
    logError stdHandle "using standard error handle"
    --error "logger fail"
    return $ resources { flHandle = S.stderr }



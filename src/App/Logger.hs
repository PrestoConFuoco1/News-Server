module App.Logger where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T (hPutStrLn)
import Prelude hiding (log)

import Control.Monad (when)
import qualified Control.Monad.Catch as C
import qualified GHC.IO.Handle.Lock as Lk
import qualified System.Exit as Q (ExitCode(..), exitWith)
import qualified System.IO as I
import qualified System.IO.Error as IOE
import qualified Utils as S

newtype Handle m =
   Handle
      { log :: Priority -> T.Text -> m ()
      }

type LoggerEntry = (Priority, T.Text)

data Priority
   = Debug
   | Info
   | Warning
   | Error
   | Fatal
   deriving (Eq, Ord, Show, Read)

logDebug, logInfo, logWarning, logError, logFatal ::
      Handle m -> T.Text -> m ()
logDebug = (`log` Debug)

logInfo = (`log` Info)

logWarning = (`log` Warning)

logError = (`log` Error)

logFatal = (`log` Fatal)



emptyLogger :: Handle IO
emptyLogger = Handle $ \_ _ -> pure ()

logString :: Priority -> T.Text -> T.Text
logString pri s = "[" <> S.showText pri <> "]: " <> s


stdHandle :: Handle IO
stdHandle = stdCondHandle $ const True

stdCondHandle :: (Priority -> Bool) -> Handle IO
stdCondHandle predicate = Handle $ \p s ->
    let h | p >= Warning = I.stderr
          | otherwise    = I.stdout
    in  when (predicate p) $ handleLogger h p s

handleLogger :: I.Handle -> Priority -> T.Text -> IO ()
handleLogger h p s = T.hPutStrLn h $ logString p s



data LoggerConfig =
   LoggerConfig
      { lcFilter :: Priority -> Bool
      , lcPath :: FilePath
      }

newtype LoggerResources =
   LoggerResources
      { flHandle :: I.Handle
      }

pathToHandle :: FilePath -> IO I.Handle
pathToHandle path = do
   h <- I.openFile path I.AppendMode
   pure h

initializeErrorHandler :: IOE.IOError -> IO a
initializeErrorHandler e = do
   logFatal stdHandle "failed to initialize logger"
   func e
   Q.exitWith (Q.ExitFailure 1)
  where
    func x
       | IOE.isAlreadyInUseError x =
          logError stdHandle lockedmsg
       | IOE.isPermissionError x =
          logError stdHandle "not enough permissions"
       | otherwise =
          logError stdHandle $
          "unexpected IO error: " <>
          T.pack (C.displayException x)

lockedmsg :: T.Text
lockedmsg = "target log file is locked"

initializeDefaultHandler :: C.SomeException -> IO a
initializeDefaultHandler e = do
   logFatal stdHandle "failed to initialize logger"
   logFatal stdHandle $ T.pack $ C.displayException e
   Q.exitWith (Q.ExitFailure 1)

withSelfSufficientLogger ::
      LoggerConfig -> (Handle IO -> IO a) -> IO a
withSelfSufficientLogger conf action = do
   C.bracket
      (initializeSelfSufficientLoggerResources conf)
      closeSelfSufficientLogger
      (\resourcesRef ->
          action $
          Handle $
          selfSufficientLogger resourcesRef $ lcFilter conf)

initializeSelfSufficientLoggerResources ::
      LoggerConfig -> IO (IORef LoggerResources)
initializeSelfSufficientLoggerResources conf = do
   h <-
      pathToHandle (lcPath conf) `C.catches`
      [ C.Handler initializeErrorHandler
      , C.Handler initializeDefaultHandler
      ]
   lockAcquired <- Lk.hTryLock h Lk.ExclusiveLock
   when (not lockAcquired) $ do
      logFatal stdHandle "failed to initialize logger"
      logFatal stdHandle lockedmsg
      Q.exitWith (Q.ExitFailure 1)
   newIORef $ LoggerResources {flHandle = h}

closeSelfSufficientLogger :: IORef LoggerResources -> IO ()
closeSelfSufficientLogger resourcesRef = do
   resources <- readIORef resourcesRef
   let h = flHandle resources
   Lk.hUnlock h
   I.hClose h

selfSufficientLogger ::
      IORef LoggerResources
   -> (Priority -> Bool)
   -> Priority
   -> T.Text
   -> IO ()
selfSufficientLogger resourcesRef predicate pri s = do
   resources <- readIORef resourcesRef
   let action =
          when (predicate pri) $ do
          T.hPutStrLn (flHandle resources) (logString pri s)
          T.hPutStrLn I.stderr (logString pri s)
       errHandler e =
          loggerHandler resources e >>=
          writeIORef resourcesRef
   action `C.catch` errHandler

loggerHandler ::
      LoggerResources
   -> C.SomeException
   -> IO LoggerResources
loggerHandler resources e = do
   logError stdHandle "failed to use log file, error is:"
   logError stdHandle $ T.pack $ C.displayException e
   logError stdHandle "using standard error handle"
   pure $ resources {flHandle = I.stderr}

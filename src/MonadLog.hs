module MonadLog where

import Prelude hiding (log)
import Types
import Database
import Control.Monad.Catch as CMC
import Control.Monad (when)
import qualified Data.Aeson as Ae
import qualified Data.Text as T
import IO.ServerIO

import Control.Monad.IO.Class (MonadIO, liftIO)

--import Result
import Handler.Logger

class (Monad m) => MonadLog m where
    logM :: LoggerEntry -> m ()



logDebug, logError, logInfo, logWarn, logFatal :: (MonadLog m) => T.Text -> m ()
logDebug s = logM (Debug, s)
logError s = logM (Error, s)
logInfo s = logM (Info, s)
logWarn s = logM (Warning, s)
logFatal s = logM (Fatal, s)



instance MonadLog ServerIO where
    logM (pri, text) = do
        s <- ask
        liftIO $ log (logger s) pri text




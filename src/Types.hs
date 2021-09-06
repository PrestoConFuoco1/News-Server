{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Types where


import qualified Logger as L
import Control.Monad.Reader (MonadReader, ReaderT, asks, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T (Text, pack, unpack)

data ServerHandlers = ServerHandlers {
    logger :: L.Handle,
    sqlHandler :: Int
    }

newtype ServerIO a = ServerIO { runServerIO :: ReaderT ServerHandlers IO a }
    deriving (Functor, Applicative, Monad, MonadReader ServerHandlers, MonadIO)

class (Monad m) => MonadSQL m where
    sql :: Int -> m Int
    
class (Monad m) => MonadLog m where
    logM :: L.LoggerEntry -> m ()

class (MonadSQL m, MonadLog m) => MonadServer m where
    runServer :: ServerHandlers -> m a -> IO a


instance MonadSQL ServerIO where
    sql = return

instance MonadLog ServerIO where
    logM (pri, text) = do
        s <- ask
        liftIO $ L.log (logger s) pri text

instance MonadServer ServerIO where
    runServer handlers = flip runReaderT handlers . runServerIO



logDebug, logError, logInfo, logWarn :: (MonadLog m) => T.Text -> m ()
logDebug s = logM (L.Debug, s)
logError s = logM (L.Error, s)
logInfo s = logM (L.Info, s)
logWarn s = logM (L.Warning, s)



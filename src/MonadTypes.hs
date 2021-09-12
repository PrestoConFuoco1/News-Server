{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module MonadTypes where


import qualified Logger as L
import Control.Monad.Reader (MonadReader, ReaderT, asks, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T (Text, pack, unpack)
import RequestToAction
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.ToField as PSF
import qualified DatabaseHandler as DB
import qualified Data.ByteString as BS

data ServerHandlers = ServerHandlers {
    logger :: L.Handle,
    sqlHandler :: DB.Handle
    }

newtype ServerIO a = ServerIO { runServerIO :: ReaderT ServerHandlers IO a }
    deriving (Functor, Applicative, Monad, MonadReader ServerHandlers, MonadIO)


{-
class (Monad m) => MonadGetPosts m where
    getPosts :: GetPosts -> m () -- Response
-}
class (Monad m) => MonadSQL m where
    query :: (PS.ToRow q, PS.FromRow r) => PS.Query -> q -> m [r]
    query_ :: (PS.FromRow r) => PS.Query -> m [r]
    formatQuery :: (PSF.ToField q) => PS.Query -> [q] -> m BS.ByteString
  
class (Monad m) => MonadLog m where
    logM :: L.LoggerEntry -> m ()

class (MonadSQL m, MonadLog m) => MonadServer m where
    runServer :: ServerHandlers -> m a -> IO a


instance MonadSQL ServerIO where
    query qu args = do
        s <- ask
        liftIO $ PS.query (DB.conn $ sqlHandler s) qu args
    query_ qu = do
        s <- ask
        liftIO $ PS.query_ (DB.conn $ sqlHandler s) qu
    formatQuery qu args = do
        s <- ask
        liftIO $ PS.formatQuery (DB.conn $ sqlHandler s) qu args


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

------------------------------------------------


--getPostsServerIO :: GetPosts -> ServerIO Response
--getPostsServerIO x = undefined



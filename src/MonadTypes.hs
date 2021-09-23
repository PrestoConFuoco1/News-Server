{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module MonadTypes where

import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Handler.Logger as L
import Control.Monad.Reader (MonadReader, ReaderT, asks, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Database.PostgreSQL.Simple as PS
import qualified Handler.Database as DB
import qualified Data.ByteString as BS

import Control.Monad.Catch as CMC
import Control.Exception as CE

import System.Random
import GHC.Arr

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
    --query_ :: (PS.FromRow r) => PS.Query -> m [r]
    --formatQuery :: (PSF.ToField q) => PS.Query -> [q] -> m BS.ByteString
    formatQuery :: (PS.ToRow q) => PS.Query -> q -> m BS.ByteString
    execute :: (PS.ToRow q) => PS.Query -> q -> m Int
  
class (Monad m) => MonadLog m where
    logM :: L.LoggerEntry -> m ()

class (MonadSQL m, MonadLog m, MonadCatch m) => MonadServer m where
    runServer :: ServerHandlers -> m a -> IO a
    randomString :: Int -> m String

    printS :: (Show a) => a -> m ()
    getCurrentTimeS :: m UTCTime

instance CMC.MonadThrow ServerIO where
    throwM e = liftIO $ CE.throwIO e

instance CMC.MonadCatch ServerIO where
    catch (ServerIO m) c = ServerIO $ m `CMC.catch` \e -> runServerIO (c e)


instance MonadSQL ServerIO where
    query qu args = do
        s <- ask
        liftIO $ PS.query (DB.conn $ sqlHandler s) qu args
    formatQuery qu args = do
        s <- ask
        liftIO $ PS.formatQuery (DB.conn $ sqlHandler s) qu args
    execute qu args = do    
        s <- ask
        res64 <- liftIO $ PS.execute (DB.conn $ sqlHandler s) qu args
        return $ fromIntegral res64


instance MonadLog ServerIO where
    logM (pri, text) = do
        s <- ask
        liftIO $ L.log (logger s) pri text

instance MonadServer ServerIO where
    runServer handlers = flip runReaderT handlers . runServerIO
    randomString int = liftIO $ randomString' int
    printS s = liftIO $ print s
    getCurrentTimeS = liftIO $ getCurrentTime

randomString' :: Int -> IO String            
randomString' int = do
        let str = "qwertyuiopasdfghjklzxcvbnm"
            len = length str
            arr = array (1, len) $ zip [1..len] str
        xs <- sequence $ replicate int $ randomRIO (1, len)
        return $ map (arr !) xs


logDebug, logError, logInfo, logWarn, logFatal :: (MonadLog m) => T.Text -> m ()
logDebug s = logM (L.Debug, s)
logError s = logM (L.Error, s)
logInfo s = logM (L.Info, s)
logWarn s = logM (L.Warning, s)
logFatal s = logM (L.Fatal, s)

------------------------------------------------





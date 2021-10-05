{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IO.ServerIO (module IO.ServerIO, ask) where


--import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Handler.Logger as L
import Control.Monad.Reader (MonadReader, ReaderT, asks, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Database.PostgreSQL.Simple as PS
import qualified Handler.Database as DB
import qualified Data.ByteString as BS

import Control.Monad.Catch as CMC
import Control.Exception as CE


data ServerHandlers = ServerHandlers {
    logger :: L.Handle,
    sqlHandler :: DB.Handle
    }

newtype ServerIO a = ServerIO { runServerIO :: ReaderT ServerHandlers IO a }
    deriving (Functor, Applicative, Monad, MonadReader ServerHandlers, MonadIO)


instance CMC.MonadThrow ServerIO where
    throwM e = liftIO $ CE.throwIO e

instance CMC.MonadCatch ServerIO where
    catch (ServerIO m) c = ServerIO $ m `CMC.catch` \e -> runServerIO (c e)

runServer :: ServerHandlers -> ServerIO a -> IO a
runServer hs m = runReaderT (runServerIO m) hs

query :: (PS.ToRow q, PS.FromRow r) => PS.Query -> q -> ServerIO [r]
query qu args = do
    s <- ask
    liftIO $ PS.query (DB.conn $ sqlHandler s) qu args
formatQuery :: (PS.ToRow q) => PS.Query -> q -> ServerIO BS.ByteString
formatQuery qu args = do
    s <- ask
    liftIO $ PS.formatQuery (DB.conn $ sqlHandler s) qu args
execute :: (PS.ToRow q) => PS.Query -> q -> ServerIO Int
execute qu args = do    
    s <- ask
    res64 <- liftIO $ PS.execute (DB.conn $ sqlHandler s) qu args
    return $ fromIntegral res64

withTransaction :: ServerIO a -> ServerIO a
withTransaction m = do
    s <- ask
    let conn = DB.conn $ sqlHandler s
    return (s :: ServerHandlers)
    liftIO $ PS.withTransaction conn $ runReaderT (runServerIO m) s


------------------------------------------------





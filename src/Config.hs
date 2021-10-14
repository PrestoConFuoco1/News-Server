{-# LANGUAGE
    RecordWildCards
    , DeriveGeneric
    #-}
module Config where


import qualified Data.ByteString as B


import qualified Control.Exception as E
    (catches, Handler (..), SomeException, IOException)
import qualified Control.Monad.Catch as C
import qualified System.IO.Error as E
    (isDoesNotExistError, isPermissionError, isAlreadyInUseError)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT

import qualified System.Exit as Q (ExitCode (..), exitWith)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified App.Logger as L
import Control.Monad (when, forever)

import Data.IORef
import App.Database as D
import qualified Utils as S (withMaybe)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import qualified Data.Text as T (pack)
import GHC.Generics
import GenericPretty

{-
data ConfigException = RequiredFieldMissing
    deriving (Show, Eq)

instance C.Exception ConfigException
-}


data Config = Config {
    databaseName :: B.ByteString
    , dbUser :: B.ByteString
    , dbPassword :: B.ByteString
    , dbAdmin :: B.ByteString
    , dbAdminPassword :: B.ByteString
    , dbPort :: Int
    } deriving (Show, Eq, Generic)

instance PrettyShow Config
{-
defaultConfig = Config {
    databaseName = "newsdb"
    , dbUser = "newsdb_app"
    , dbPassword = "0000"
    , dbAdmin = "newsdb_owner"
    , dbAdminPassword = "0000"
    , dbPort = 5555
    }
-}
defaultPort = 5555

{-
appConnectionString :: Config -> B.ByteString
appConnectionString Config {..} =
    "dbname=" <> databaseName <>
    " user=" <> dbUser <>
    " password='" <> dbPassword <> "'"
-}
{-
adminConnectionString :: Config -> B.ByteString
adminConnectionString Config {..} =
    "dbname=" <> databaseName <>
    " user=" <> dbAdmin <>
    " password='" <> dbAdminPassword <> "'"
-}

loadConfig :: L.Handle IO -> FilePath -> IO Config
loadConfig handle path = do
    conf <- C.load [CT.Required path]
    database <- C.require conf "database_name"
    user <- C.require conf "app_user"
    userPasswd <- C.require conf "app_user_password"
    admin <- C.require conf "admin"
    adminPasswd <- C.require conf "admin_password"
    port <- C.lookupDefault defaultPort conf "port"
    let config = Config {
        databaseName = database
        , dbUser = user
        , dbPassword = userPasswd
        , dbAdmin = admin
        , dbAdminPassword = adminPasswd
        , dbPort = port
        }
    L.logInfo handle $ "successfully got server configuration"
    L.logDebug handle $ textPretty config
    return config
    

configHandlers :: L.Handle IO -> [C.Handler IO a]
configHandlers h = 
    let f (C.Handler g) = C.Handler (\e -> g e >>
            L.logFatal h
                "Failed to get required data from configuration files, terminating..."
            >> Q.exitWith (Q.ExitFailure 1))
    in  map f [
                C.Handler (handleIOError h)
                , C.Handler (handleConfigError h)
                , C.Handler (handleKeyError h)
                , C.Handler (handleOthers h)
              ]
 

handleIOError :: L.Handle IO -> E.IOException -> IO ()
handleIOError logger exc
  | E.isDoesNotExistError exc = L.logError logger "File does not exist."
  | E.isPermissionError exc   = L.logError logger "Not enough permissions to open file."
  | E.isAlreadyInUseError exc = L.logError logger "File is already in use."
  | otherwise = do
        L.logError logger "Unknown IOError occured"
        L.logError logger $ T.pack $ C.displayException exc

handleConfigError :: L.Handle IO -> CT.ConfigError -> IO ()
handleConfigError logger (CT.ParseError path s) =
    L.logError logger $ "Failed to parse configuration file."
{-
handleConfigError logger e = do
    L.logError logger $ "Unknown ConfigError occured"
    L.logError logger $ T.pack $ C.displayException e
-- this pattern is redundant because ConfigError has only one constructor
-}

handleKeyError :: L.Handle IO -> CT.KeyError -> IO ()
handleKeyError logger (CT.KeyError name) = do
    L.logError logger $ "No field with name " <> name <> " found."

handleOthers :: L.Handle IO -> E.SomeException -> IO ()
handleOthers logger exc = do
    L.logError logger "Unknown error occured."
    L.logError logger $ T.pack $ C.displayException exc



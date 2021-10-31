{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Config
    ( Config(..)
    , loadConfig
    , configHandlers
    ) where

import qualified App.Logger as L
import qualified Control.Exception as E (IOException, SomeException)
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as Text (pack)
import GHC.Generics
import qualified GenericPretty as GP
import qualified System.Exit as Q (ExitCode(..), exitWith)
import qualified System.IO.Error as E
    ( isAlreadyInUseError
    , isDoesNotExistError
    , isPermissionError
    )

data Config =
    Config
        { databaseName :: B.ByteString
        , dbUser :: B.ByteString
        , dbPassword :: B.ByteString
        , dbAdmin :: B.ByteString
        , dbAdminPassword :: B.ByteString
        , dbPort :: Int
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (GP.PrettyShow)

defaultPort :: Int
defaultPort = 5555

loadConfig :: L.LoggerHandler IO -> FilePath -> IO Config
loadConfig handle path = do
    conf <- C.load [CT.Required path]
    database <- C.require conf "database_name"
    user <- C.require conf "app_user"
    userPasswd <- C.require conf "app_user_password"
    admin <- C.require conf "admin"
    adminPasswd <- C.require conf "admin_password"
    port <- C.lookupDefault defaultPort conf "port"
    let config =
            Config
                { databaseName = database
                , dbUser = user
                , dbPassword = userPasswd
                , dbAdmin = admin
                , dbAdminPassword = adminPasswd
                , dbPort = port
                }
    L.logInfo handle "successfully got server configuration"
    L.logDebug handle $ GP.textPretty config
    pure config

configHandlers :: L.LoggerHandler IO -> [C.Handler IO a]
configHandlers h =
    map (logThenTerminate h)
        [ C.Handler (handleIOError h)
        , C.Handler (handleConfigError h)
        , C.Handler (handleKeyError h)
        , C.Handler (handleOthers h)
        ]

logThenTerminate ::
       L.LoggerHandler IO -> C.Handler IO a -> C.Handler IO b
logThenTerminate logger (C.Handler g) =
    let msg =
            "Failed to get required data from configuration files, terminating..."
     in C.Handler $ \e -> do
            _ <- g e
            L.logFatal logger msg
            Q.exitWith $ Q.ExitFailure 1

handleIOError :: L.LoggerHandler IO -> E.IOException -> IO ()
handleIOError logger exc
    | E.isDoesNotExistError exc =
        L.logError logger "File does not exist."
    | E.isPermissionError exc =
        L.logError logger "Not enough permissions to open file."
    | E.isAlreadyInUseError exc =
        L.logError logger "File is already in use."
    | otherwise = do
        L.logError logger "Unknown IOError occured"
        L.logError logger $ Text.pack $ C.displayException exc

handleConfigError :: L.LoggerHandler IO -> CT.ConfigError -> IO ()
handleConfigError logger (CT.ParseError _ _) =
    L.logError logger "Failed to parse configuration file."

handleKeyError :: L.LoggerHandler IO -> CT.KeyError -> IO ()
handleKeyError logger (CT.KeyError name) = do
    L.logError logger $ "No field with name " <> name <> " found."

handleOthers :: L.LoggerHandler IO -> E.SomeException -> IO ()
handleOthers logger exc = do
    L.logError logger "Unknown error occured."
    L.logError logger $ Text.pack $ C.displayException exc

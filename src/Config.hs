{-# LANGUAGE RecordWildCards, DeriveGeneric #-}

module Config where

import qualified App.Logger as L
import qualified Control.Exception as E
   ( IOException
   , SomeException
   )
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T (pack)
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

instance GP.PrettyShow Config

defaultPort :: Int
defaultPort = 5555

loadConfig :: L.Handle IO -> FilePath -> IO Config
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

configHandlers :: L.Handle IO -> [C.Handler IO a]
configHandlers h =
   let f (C.Handler g) =
          C.Handler
             (\e -> do
                 g e
                 L.logFatal
                    h
                    "Failed to get required data from configuration files, terminating..."
                 Q.exitWith (Q.ExitFailure 1))
    in map
          f
          [ C.Handler (handleIOError h)
          , C.Handler (handleConfigError h)
          , C.Handler (handleKeyError h)
          , C.Handler (handleOthers h)
          ]

handleIOError :: L.Handle IO -> E.IOException -> IO ()
handleIOError logger exc
   | E.isDoesNotExistError exc =
      L.logError logger "File does not exist."
   | E.isPermissionError exc =
      L.logError
         logger
         "Not enough permissions to open file."
   | E.isAlreadyInUseError exc =
      L.logError logger "File is already in use."
   | otherwise = do
      L.logError logger "Unknown IOError occured"
      L.logError logger $ T.pack $ C.displayException exc

handleConfigError :: L.Handle IO -> CT.ConfigError -> IO ()
handleConfigError logger (CT.ParseError _ _) =
   L.logError logger "Failed to parse configuration file."

handleKeyError :: L.Handle IO -> CT.KeyError -> IO ()
handleKeyError logger (CT.KeyError name) = do
   L.logError logger $
      "No field with name " <> name <> " found."

handleOthers :: L.Handle IO -> E.SomeException -> IO ()
handleOthers logger exc = do
   L.logError logger "Unknown error occured."
   L.logError logger $ T.pack $ C.displayException exc

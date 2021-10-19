{-# LANGUAGE
    TemplateHaskell
    , RecordWildCards
    #-}
module Migrations where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import qualified Data.ByteString as BS
import qualified Data.List as L (sortBy)
import Data.FileEmbed
import Data.Function (on)
import System.Exit
import Control.Monad (forM_)


data Config = Config {
    databaseName :: BS.ByteString
    , adminName :: BS.ByteString
    , adminPassword :: BS.ByteString
    } deriving (Show)


adminConnectionString :: Config -> BS.ByteString
adminConnectionString Config {..} =
    "dbname=" <> databaseName <>
    " user=" <> adminName <>
    " password='" <> adminPassword <> "'"


migrationMain :: Config -> IO ()
migrationMain conf = do
    let conStr = adminConnectionString conf
    con <- connectPostgreSQL conStr
    runMigrations1 con


sortedMigrations :: [(FilePath, BS.ByteString)]
sortedMigrations =
    let unsorted = $(embedDir "migrations")
    in  L.sortBy (compare `on` fst) unsorted

runMigrations1 :: Connection -> IO ()
runMigrations1 con =
 withTransaction con $ do
    let defaultContext =
            MigrationContext
            { migrationContextCommand = MigrationInitialization,
              migrationContextVerbose = True,
              migrationContextConnection = con
            }
        migrations =("(init)", defaultContext) :
                    [
                        (k, defaultContext
                            { migrationContextCommand =
                                MigrationScript k v
                            })
                        | (k, v) <- sortedMigrations
                    ]
    forM_ migrations $ \(migrDescr, migr) -> do
 --       putStrLn migrDescr >> BS.putStrLn migr
        res <- runMigration migr
        case res of
          MigrationSuccess -> return ()
          MigrationError reason -> exitFailure















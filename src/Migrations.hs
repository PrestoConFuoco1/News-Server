{-# LANGUAGE TemplateHaskell #-}
module Migrations where



--import qualified Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple
--import qualified Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.Migration
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.FileEmbed
import Data.Function
import System.Exit
import Control.Monad


migrationMain :: IO ()
--migrationMain = mapM_ (\(f, s) -> putStrLn f >> BS.putStrLn s) sortedMigrations
{-
migrationMain = do
    con <- connectPostgreSQL "dbname=migration2 user=migration2_owner password='0000'"
    withTransaction con $ runMigration $
        MigrationContext MigrationInitialization True con
    return ()
-}

migrationMain = do
    con <- connectPostgreSQL "dbname=newsdb user=newsdb_owner password='0000'"
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















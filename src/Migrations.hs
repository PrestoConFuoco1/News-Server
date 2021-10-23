{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module Migrations where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.FileEmbed
import Data.Function (on)
import qualified Data.List as L (sortBy)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Exit

data Config =
   Config
      { databaseName :: BS.ByteString
      , adminName :: BS.ByteString
      , adminPassword :: BS.ByteString
      }
   deriving (Show)

adminConnectionString :: Config -> BS.ByteString
adminConnectionString Config {..} =
   "dbname=" <>
   databaseName <>
   " user=" <>
   adminName <> " password='" <> adminPassword <> "'"

migrationMain :: Config -> IO ()
migrationMain conf = do
   let conStr = adminConnectionString conf
   con <- connectPostgreSQL conStr
   runMigrations1 con

sortedMigrations :: [(FilePath, BS.ByteString)]
sortedMigrations =
   let unsorted = $(embedDir "migrations")
    in L.sortBy (compare `on` fst) unsorted

runMigrations1 :: Connection -> IO ()
runMigrations1 con =
   withTransaction con $ do
      let defaultContext =
             MigrationContext
                { migrationContextCommand =
                     MigrationInitialization
                , migrationContextVerbose = True
                , migrationContextConnection = con
                }
          migrations =
             ("(init)", defaultContext) :
             [ ( k
               , defaultContext
                    { migrationContextCommand =
                         MigrationScript k v
                    })
             | (k, v) <- sortedMigrations
             ]
      forM_ migrations $ \(_, migr)
 --       putStrLn migrDescr >> BS.putStrLn migr
       -> do
         res <- runMigration migr
         case res of
            MigrationSuccess -> pure ()
            MigrationError _ -> exitFailure

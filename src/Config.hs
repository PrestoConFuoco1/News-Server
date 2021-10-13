{-# LANGUAGE
    RecordWildCards
    #-}
module Config where


import Data.ByteString

data Config = Config {
    databaseName :: ByteString
    , dbUser :: ByteString
    , dbPassword :: ByteString
    , dbAdmin :: ByteString
    , dbAdminPassword :: ByteString

    } deriving (Show, Eq)

defaultConfig = Config {
    databaseName = "newsdb"
    , dbUser = "newsdb_app"
    , dbPassword = "0000"
    , dbAdmin = "newsdb_owner"
    , dbAdminPassword = "0000"

    }

appConnectionString :: Config -> ByteString
appConnectionString Config {..} =
    "dbname=" <> databaseName <>
    " user=" <> dbUser <>
    " password='" <> dbPassword <> "'"

adminConnectionString :: Config -> ByteString
adminConnectionString Config {..} =
    "dbname=" <> databaseName <>
    " user=" <> dbAdmin <>
    " password='" <> dbAdminPassword <> "'"


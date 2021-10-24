{-# LANGUAGE DeriveGeneric #-}
module RunOptions where

import GHC.Generics
import qualified App.Logger as L
import qualified Utils as S (withMaybe)
import Text.Read (readMaybe)
import Data.List (isPrefixOf)
import Options.Applicative
import Data.Maybe (fromMaybe)
import qualified GenericPretty as P
import qualified Data.Text as T
import Types

data LoggerSettings =
    LogAll
    | LogGreaterThan L.Priority
    deriving (Show, Eq)
instance P.PrettyShow LoggerSettings where
    prettyShow = P.LStr . show

toLoggerFilter :: LoggerSettings -> (L.Priority -> Bool)
toLoggerFilter LogAll = const True
toLoggerFilter (LogGreaterThan pri) = (>= pri)

data RunOptions =
   RunOptions
      { confPath :: T.Text
      , migrations :: Bool
      , loggerSettings :: LoggerSettings
      , logPath :: T.Text
      , testConfig :: Bool
      } deriving (Show, Eq, Generic)
instance P.PrettyShow RunOptions

--for ghci
ghciRunOpts :: RunOptions
ghciRunOpts =
   RunOptions
      { confPath = "./server.conf"
      , migrations = False
      , loggerSettings = LogAll
      , logPath = "./log"
      , testConfig = False
      }

getOpts :: Parser RunOptions
getOpts = RunOptions
    <$> argument str (metavar "CONFPATH")
    <*> switch (short 'm' <> long "migrations" <> help "Whether to run migrations")
    <*> (
        (getLoggerSettings <$> option auto ( short 'l' <> metavar "LOGLEVEL" <> help "Log level" ))
        <|> pure LogAll)
    <*> ( 
        strOption ( long "logpath" <> metavar "LOGFILE" <> help "Log path")
        <|> pure "./log")
    <*> switch (long "test-config" <> help "Test configuration")

serverHeader = "Backend for news server"

getOptsIO :: IO RunOptions
getOptsIO = execParser $ info (getOpts <**> helper)
    ( fullDesc
    <> header   serverHeader )

getLoggerSettings :: L.Priority -> LoggerSettings
getLoggerSettings = LogGreaterThan


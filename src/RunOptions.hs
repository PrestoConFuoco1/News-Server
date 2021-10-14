module RunOptions where

import Utils as S
import qualified App.Logger as L (simpleLog, logDebug, Priority)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)


data RunOptions = RunOptions {
    loggerSettings :: L.Priority -> Bool
    , testConfig :: Bool
    , logPath :: FilePath
    , migrations :: Bool
    }
defaultRunOpts = RunOptions {
    loggerSettings = const True
    , testConfig = False
    , logPath = "./log"
    , migrations = False
    }


getOpts :: [String] -> RunOptions
getOpts = foldr f defaultRunOpts
  where logpath = "--logpath=" :: String
        logPathLength = length logpath
--        f "--test-config" acc = acc { testConfig = True }
        f "-m" acc = acc { migrations = True }
        f "--test-config" acc = acc { testConfig = True }
        f str acc
            | logpath `isPrefixOf` str =
                acc { logPath = drop logPathLength str }
            | "-l" `isPrefixOf` str =
                S.withMaybe (getLoggerSettings $ drop 2 str)
                    acc (\x -> acc { loggerSettings = x })
        f _ acc = acc


getLoggerSettings :: String -> Maybe (L.Priority -> Bool)
getLoggerSettings str = fmap (\x -> (>= x)) $ readMaybe str


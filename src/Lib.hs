{-# LANGUAGE
    RecordWildCards
    #-}

module Lib
    ( someFunc
    ) where

import Data.IORef
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai as W (Request(..), Response, Application, responseLBS)
import qualified GenericPretty as GP (textPretty)
import Action.RequestToAction (requestToAction)
import Execute (executeAction, handleError)
import Types
import Result
import qualified App.Logger as L
import qualified Data.Aeson as Ae (encode, ToJSON(..))
import qualified Exceptions as Ex (mainErrorHandler, defaultMainHandler)
import qualified Control.Monad.Catch as CMC
import qualified Migrations as M
import RunOptions
import qualified App.Database as D
import qualified App.Database.Postgres as DP
import qualified Config as C
import System.Exit as Q
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import qualified Utils as S

someFunc :: IO ()
someFunc = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Expected path to configuration file." >>
                Q.exitWith (Q.ExitFailure 1)
        (x:xs) -> runWithConf (getOpts xs) x

runWithConf :: RunOptions -> FilePath -> IO ()
runWithConf opts path = do
    let configLogger = L.stdHandle
    conf <- C.loadConfig configLogger path `CMC.catches` C.configHandlers configLogger
    when (testConfig opts) $ exitSuccess
    if migrations opts
        then M.migrationMain $ configToMigrationsConfig conf
        else let loggerSettings = runOptsToLoggerSettings opts
             in  someFunc1 loggerSettings $ configToAppConfig conf

runOptsToLoggerSettings :: RunOptions -> L.LoggerConfig
runOptsToLoggerSettings opts = L.LoggerConfig {
    lcFilter = loggerSettings opts
    , lcPath = logPath opts
    }

configToAppConfig :: C.Config -> DP.Config
configToAppConfig C.Config {..} = DP.Config {
    DP.databaseName = databaseName
    , DP.userName = dbUser
    , DP.password = dbPassword
    , DP.port = dbPort
    }

configToMigrationsConfig :: C.Config -> M.Config
configToMigrationsConfig C.Config {..} = M.Config {
    M.databaseName = databaseName
    , M.adminName = dbAdmin
    , M.adminPassword = dbAdminPassword
    }

someFunc1 :: L.LoggerConfig -> DP.Config -> IO ()
someFunc1 loggerConfig conf1 = do
    L.withSelfSufficientLogger loggerConfig $ \logger ->
        DP.withPostgresHandle L.stdHandle conf1 $ \resources -> do
            resourcesRef <- newIORef resources
            Warp.run (DP.port conf1) $ mainFunc1 logger resourcesRef



mainFunc1 :: L.Handle IO -> IORef DP.Resources -> W.Application
mainFunc1 logger resourcesRef req respond = do
    resources <- readIORef resourcesRef
    (response, resources') <- mainServer req logger resources
    writeIORef resourcesRef resources'
    respond response

mainServer :: W.Request -> L.Handle IO -> DP.Resources -> IO (W.Response, DP.Resources)
mainServer req logger resources = do
    let
 --       logger = L.stdHandle
        h = DP.resourcesToHandle resources logger
        f x = x >>= \q -> return (q, resources)
    D.logDebug h ""
    D.logDebug h "Got request"
    D.logDebug h $ ("Path: " <>) $ S.showText $ W.pathInfo req
    D.logDebug h $ ("Args: " <>) $ S.showText $ W.queryString req
    let eithWhoWhat = requestToAction req

    case eithWhoWhat of
        Left err -> f $ coerceResponse <$> handleError h err
        Right whowhat -> do
            D.logDebug h "Action type is"
            D.logDebug h $ GP.textPretty $ _ww_action whowhat

            let withLog res = do
                    r <- res
                    D.logDebug h "Result is"
                    D.logDebug h $ logResult r
                    return $ toResponse r
                action = withLog (executeAction h whowhat)
                    `CMC.catches` [CMC.Handler $ Ex.mainErrorHandler logger,
                                   CMC.Handler $ Ex.defaultMainHandler logger]
                action' = fmap coerceResponse action
            --action' >>= \x -> (x, resources)
            f action'



coerceResponse :: Response -> W.Response
coerceResponse (Response status msg) =
    W.responseLBS status [] $ Ae.encode msg


toResponse :: APIResult -> Response
toResponse (RGet (RGettable xs)) = ok successGet (Ae.toJSON xs)
toResponse (RGetUser user) = ok successGetProfile (Ae.toJSON user)
toResponse (RGetToken tok) = ok successNewToken (Ae.toJSON tok)
toResponse (RCreated ent int) = ok (createdMsg ent) (Ae.toJSON int)
toResponse (REdited ent int) = ok (editedMsg ent) (Ae.toJSON int)
toResponse (RDeleted ent int) = ok (deletedMsg ent) (Ae.toJSON int)
toResponse (RNotFound ent) = bad (entityNotFoundMsg ent)
toResponse (RAlreadyInUse ent field value) = bad (alreadyInUseMsg ent field value)
toResponse (RInvalidForeign _ field value) = bad (invalidForeignMsg field value)
toResponse (RInvalidTag value) = bad (tagNotFoundMsg value)





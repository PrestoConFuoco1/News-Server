{-# LANGUAGE RecordWildCards #-}

module Lib
    ( main
    ) where

import Action.RequestToAction (requestToActionHTTP)
import qualified App.Database as D
import qualified App.Database.Postgres as DP
import qualified App.Logger as L
import qualified Config as C
import Control.Monad (when)
import qualified Control.Monad.Catch as CMC
import qualified Data.Aeson as Ae (ToJSON(..), encode)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T (unpack)
import qualified Exceptions as Ex
    ( defaultMainHandler
    , mainErrorHandler
    )
import Execute (executeAction, handleError)
import qualified GenericPretty as GP (textPretty)
import qualified Migrations as M
import qualified Network.Wai as W
    ( Application
    , Request(..)
    , Response
    , responseLBS
    )
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Result as R
import qualified RunOptions as Opt
import qualified System.Exit as Q
import qualified Types as Y
import qualified Utils as S

main :: IO ()
main = do
    opts <- Opt.getOptsIO
    L.logDebug L.stdHandler $ GP.textPretty opts
    runWithOpts opts

runWithOpts :: Opt.RunOptions -> IO ()
runWithOpts opts = do
    let configLogger = L.stdHandler
    conf <-
        C.loadConfig configLogger (T.unpack $ Opt.confPath opts) `CMC.catches`
        C.configHandlers configLogger
    when (Opt.testConfig opts) Q.exitSuccess
    if Opt.migrations opts
        then M.migrationMain $ configToMigrationsConfig conf
        else runWithResources
                 (runOptsToLoggerSettings opts)
                 (configToAppConfig conf)

runOptsToLoggerSettings :: Opt.RunOptions -> L.LoggerConfig
runOptsToLoggerSettings opts =
    L.LoggerConfig
        { lcFilter = Opt.toLoggerFilter $ Opt.loggerSettings opts
        , lcPath = T.unpack $ Opt.logPath opts
        }

configToAppConfig :: C.Config -> DP.Config
configToAppConfig C.Config {..} =
    DP.Config
        { DP.databaseName = databaseName
        , DP.userName = dbUser
        , DP.password = dbPassword
        , DP.port = dbPort
        }

configToMigrationsConfig :: C.Config -> M.Config
configToMigrationsConfig C.Config {..} =
    M.Config
        { M.databaseName = databaseName
        , M.adminName = dbAdmin
        , M.adminPassword = dbAdminPassword
        }

runWithResources :: L.LoggerConfig -> DP.Config -> IO ()
runWithResources loggerConfig conf = do
    L.withSelfSufficientLogger loggerConfig $ \logger ->
        DP.withPostgresHandle L.stdHandler conf $ \resources -> do
            resourcesRef <- newIORef resources
            Warp.run (DP.port conf) $ mainFunc logger resourcesRef

mainFunc :: L.LoggerHandler IO -> IORef DP.Resources -> W.Application
mainFunc logger resourcesRef req respond = do
    resources <- readIORef resourcesRef
    (response, resources') <- mainServer req logger resources
    writeIORef resourcesRef resources'
    respond response

mainServer ::
       W.Request
    -> L.LoggerHandler IO
    -> DP.Resources
    -> IO (W.Response, DP.Resources)
mainServer req logger resources = do
    let h = DP.resourcesToHandle resources logger
        resourcesUnchanged x = x >>= \q -> pure (q, resources)
    D.logDebug h ""
    D.logDebug h "Got request"
    D.logDebug h $ ("Path: " <>) $ S.showText $ W.pathInfo req
    D.logDebug h $ ("Args: " <>) $ S.showText $ W.queryString req
    let eithWhoWhat = requestToActionHTTP req
    case eithWhoWhat of
        Left err ->
            resourcesUnchanged $ coerceResponse <$> handleError h err
        Right whowhat -> do
            D.logDebug h "Action type is"
            D.logDebug h $ GP.textPretty $ Y._ww_action whowhat
            let withLog res = do
                    r <- res
                    D.logDebug h "Result is"
                    D.logDebug h $ Y.logResult r
                    pure $ toResponse r
                action =
                    fmap
                        coerceResponse
                        (withLog (executeAction h whowhat) `CMC.catches`
                         [ CMC.Handler $ Ex.mainErrorHandler logger
                         , CMC.Handler $ Ex.defaultMainHandler logger
                         ])
            resourcesUnchanged action

coerceResponse :: R.Response -> W.Response
coerceResponse (R.Response status msg) =
    W.responseLBS status [] $ Ae.encode msg

toResponse :: Y.APIResult -> R.Response
toResponse (Y.RGet (Y.RGettable xs)) =
    R.ok R.successGet (Ae.toJSON xs)
toResponse (Y.RGetUser user) =
    R.ok R.successGetProfile (Ae.toJSON user)
toResponse (Y.RGetToken tok) = R.ok R.successNewToken (Ae.toJSON tok)
toResponse (Y.RCreated ent int) =
    R.ok (R.createdMsg ent) (Ae.toJSON int)
toResponse (Y.REdited ent int) =
    R.ok (R.editedMsg ent) (Ae.toJSON int)
toResponse (Y.RDeleted ent int) =
    R.ok (R.deletedMsg ent) (Ae.toJSON int)
toResponse (Y.RInvalidTag value) = R.bad (R.tagNotFoundMsg value)
toResponse (Y.RFailed ent modifError) = modifyErrorToApiResponse ent modifError

modifyErrorToApiResponse :: Y.Entity -> Y.ModifyError -> R.Response
modifyErrorToApiResponse ent (Y.MAlreadyInUse (Y.UniqueViolation field value)) =
    R.bad $ R.alreadyInUseMsg ent field value
modifyErrorToApiResponse ent (Y.MInvalidForeign (Y.ForeignViolation field value)) =
    R.bad $ R.invalidForeignMsg field value
modifyErrorToApiResponse ent (Y.MConstraintViolated (Y.ConstraintViolation field value description)) =
    R.bad $ R.constraintViolatedMsg field value description
modifyErrorToApiResponse ent Y.MNoAction =
    R.bad $ R.entityNotFoundMsg ent



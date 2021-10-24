{-# LANGUAGE RecordWildCards #-}

module Lib
   ( main
   ) where

import Action.RequestToAction (requestToAction)
import qualified App.Database as D
import qualified App.Database.Postgres as DP
import qualified App.Logger as L
import qualified Config as C
import Control.Monad (when)
import qualified Control.Monad.Catch as CMC
import qualified Data.Aeson as Ae (ToJSON(..), encode)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
import System.Environment (getArgs)
import System.Exit as Q
import System.IO (hPutStrLn, stderr)
import qualified Types as T
import qualified Utils as S
import qualified Data.Text as T (unpack)

main :: IO ()
main = do
    opts <- Opt.getOptsIO
    L.logDebug L.stdHandle $ GP.textPretty opts
    runWithOpts opts



runWithOpts :: Opt.RunOptions -> IO ()
runWithOpts opts = do
   let configLogger = L.stdHandle
   conf <-
      C.loadConfig configLogger (T.unpack $ Opt.confPath opts) `CMC.catches`
      C.configHandlers configLogger
   when (Opt.testConfig opts) $ exitSuccess
   if Opt.migrations opts
      then M.migrationMain $ configToMigrationsConfig conf
      else let loggerSettings = runOptsToLoggerSettings opts
            in someFunc1 loggerSettings $
               configToAppConfig conf

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

someFunc1 :: L.LoggerConfig -> DP.Config -> IO ()
someFunc1 loggerConfig conf1 = do
   L.withSelfSufficientLogger loggerConfig $ \logger ->
      DP.withPostgresHandle L.stdHandle conf1 $ \resources -> do
         resourcesRef <- newIORef resources
         Warp.run (DP.port conf1) $
            mainFunc1 logger resourcesRef

mainFunc1 ::
      L.Handle IO -> IORef DP.Resources -> W.Application
mainFunc1 logger resourcesRef req respond = do
   resources <- readIORef resourcesRef
   (response, resources') <- mainServer req logger resources
   writeIORef resourcesRef resources'
   respond response

mainServer ::
      W.Request
   -> L.Handle IO
   -> DP.Resources
   -> IO (W.Response, DP.Resources)
mainServer req logger resources
 = do
   let h = DP.resourcesToHandle resources logger
       f x = x >>= \q -> pure (q, resources)
   D.logDebug h ""
   D.logDebug h "Got request"
   D.logDebug h $
      ("Path: " <>) $ S.showText $ W.pathInfo req
   D.logDebug h $
      ("Args: " <>) $ S.showText $ W.queryString req
   let eithWhoWhat = requestToAction req
   case eithWhoWhat of
      Left err -> f $ coerceResponse <$> handleError h err
      Right whowhat -> do
         D.logDebug h "Action type is"
         D.logDebug h $ GP.textPretty $ T._ww_action whowhat
         let withLog res = do
                r <- res
                D.logDebug h "Result is"
                D.logDebug h $ T.logResult r
                pure $ toResponse r
             action =
                withLog (executeAction h whowhat) `CMC.catches`
                [ CMC.Handler $ Ex.mainErrorHandler logger
                , CMC.Handler $ Ex.defaultMainHandler logger
                ]
             action' = fmap coerceResponse action
         f action'

coerceResponse :: R.Response -> W.Response
coerceResponse (R.Response status msg) =
   W.responseLBS status [] $ Ae.encode msg

toResponse :: T.APIResult -> R.Response
toResponse (T.RGet (T.RGettable xs)) =
   R.ok R.successGet (Ae.toJSON xs)
toResponse (T.RGetUser user) =
   R.ok R.successGetProfile (Ae.toJSON user)
toResponse (T.RGetToken tok) =
   R.ok R.successNewToken (Ae.toJSON tok)
toResponse (T.RCreated ent int) =
   R.ok (R.createdMsg ent) (Ae.toJSON int)
toResponse (T.REdited ent int) =
   R.ok (R.editedMsg ent) (Ae.toJSON int)
toResponse (T.RDeleted ent int) =
   R.ok (R.deletedMsg ent) (Ae.toJSON int)
toResponse (T.RNotFound ent) = R.bad (R.entityNotFoundMsg ent)
toResponse (T.RAlreadyInUse ent field value) =
   R.bad (R.alreadyInUseMsg ent field value)
toResponse (T.RInvalidForeign _ field value) =
   R.bad (R.invalidForeignMsg field value)
toResponse (T.RInvalidTag value) = R.bad (R.tagNotFoundMsg value)

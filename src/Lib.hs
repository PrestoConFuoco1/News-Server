

module Lib
    ( someFunc
    ) where


import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai as W (Request(..), Response, Application, responseLBS)
import qualified Data.Text as T (pack)
--import Control.Exception

import qualified GenericPretty as GP (defaultPretty)

import Action.RequestToAction (requestToAction)
import Execute (executeAction, handleError)
import Types
import Result

import qualified Handler.Logger as L (simpleLog)
import MonadLog
import qualified Database.PostgreSQL.Simple as PS (connectPostgreSQL, Connection, close)
import qualified Handler.Database as DB (Handle(..))
import qualified Data.Aeson as Ae (encode)

import qualified Exceptions as Ex (mainErrorHandler, defaultMainHandler)
import qualified Control.Monad.Catch as CMC

import qualified Database.PostgreSQL.Simple.Migration as PSM
import Migrations

import MonadNews
import IO.ServerIO

port :: Int
port = 5555

someFunc :: IO ()
someFunc = migrationMain >> someFunc1

someFunc1 :: IO ()
someFunc1 = do
    CMC.bracket 
        --(PS.connectPostgreSQL "dbname='batadase'")
        --(PS.connectPostgreSQL "dbname=batadase user=app password='789456123'")
        (PS.connectPostgreSQL "dbname=migration2 user=migration2_app password='0000'")
        (\conn -> PS.close conn) -- close connection
        (\conn -> let serverH = ServerHandlers L.simpleLog (DB.Handle conn) in
         Warp.run port $ mainFunc1 serverH)

connectToDB :: IO PS.Connection
connectToDB = PS.connectPostgreSQL "dbname='batadase'"

mainFunc1 :: ServerHandlers -> W.Application
mainFunc1 handlers req respond = do
    response <- runServer handlers $ (mainServer req :: ServerIO W.Response)
    respond response

mainServer :: MonadNews m => W.Request -> m W.Response
mainServer req = fmap coerceResponse $ do
    logDebug $ ("Path: " <>) $ T.pack $ show $ W.pathInfo req
    logDebug $ ("Args: " <>) $ T.pack $ show $ W.queryString req
    let eithWhoWhat = requestToAction req
    
    case eithWhoWhat of
        Left err -> handleError err
        Right whowhat -> do
            logDebug "Action type is"
            logDebug $ T.pack $ GP.defaultPretty $ _ww_action whowhat

            fmap toResponse (executeAction whowhat)
                `CMC.catches` [CMC.Handler Ex.mainErrorHandler,
                               CMC.Handler Ex.defaultMainHandler]
            


coerceResponse :: Response -> W.Response
coerceResponse (Response status msg) =
    W.responseLBS status [] $ Ae.encode msg


toResponse :: APIResult -> Response
toResponse = undefined



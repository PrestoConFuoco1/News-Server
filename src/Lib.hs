

module Lib
    ( someFunc
    ) where



import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai as W (Request, Response, Application, responseLBS)
import qualified Data.Text as T (pack)
--import Control.Exception

import qualified GenericPretty as GP (defaultPretty)

import Action.RequestToAction (requestToAction)
import Action.Types (WhoWhat(..))
import Execute (executeAction, handleError)
import ExecuteTypes (Response(..))

import qualified Logger as L (simpleLog)
import MonadTypes (MonadServer, ServerIO, ServerHandlers(..), logDebug, runServer)
import qualified Database.PostgreSQL.Simple as PS (connectPostgreSQL, Connection, close)
import qualified DatabaseHandler as DB (Handle(..))
import qualified Data.Aeson as Ae (encode)

import qualified Exceptions as Ex
import qualified Control.Monad.Catch as CMC

port :: Int
port = 5555

someFunc :: IO ()
someFunc = do
 --   conn <- PS.connectPostgreSQL "dbname='batadase'"
 --   let serverH = ServerHandlers L.simpleLog (DB.Handle conn)
 --   Warp.run port $ mainFunc1 serverH

    CMC.bracket 
        (PS.connectPostgreSQL "dbname='batadase'")
        (\conn -> let serverH = ServerHandlers L.simpleLog (DB.Handle conn) in
         Warp.run port $ mainFunc1 serverH)
        (\conn -> PS.close conn) -- close connection
         

connectToDB :: IO PS.Connection
connectToDB = PS.connectPostgreSQL "dbname='batadase'"

mainFunc1 :: ServerHandlers -> W.Application
mainFunc1 handlers req respond = do
    response <- runServer handlers $ (mainServer req :: ServerIO W.Response)
    respond response


mainServer :: MonadServer m => W.Request -> m W.Response
mainServer req = fmap coerceResponse $ do
    logDebug $ T.pack $ show req
    let eithWhowhat = requestToAction req
    case eithWhowhat of
        Left err -> handleError err
        Right whowhat -> do
            logDebug "Action type is"
            logDebug $ T.pack $ GP.defaultPretty $ _ww_action whowhat

            val <- executeAction whowhat
                `CMC.catches` [CMC.Handler Ex.mainErrorHandler,
                               CMC.Handler Ex.defaultMainHandler]
            return val

coerceResponse :: Response -> W.Response
coerceResponse (Response status msg) =
    W.responseLBS status [] $ Ae.encode msg



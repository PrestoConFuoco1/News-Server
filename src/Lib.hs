

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
import Execute.Types (Response(..))

import qualified Handler.Logger as L (simpleLog)
import MonadTypes (MonadServer, ServerIO, ServerHandlers(..), logDebug, runServer, printS, getCurrentTimeS)
import qualified Database.PostgreSQL.Simple as PS (connectPostgreSQL, Connection, close)
import qualified Handler.Database as DB (Handle(..))
import qualified Data.Aeson as Ae (encode)

import qualified Exceptions as Ex (mainErrorHandler, defaultMainHandler)
import qualified Control.Monad.Catch as CMC
import Profiling

port :: Int
port = 5555

someFunc :: IO ()
someFunc = do
{-
    conn <- PS.connectPostgreSQL "dbname='batadase'"
    let serverH = ServerHandlers L.simpleLog (DB.Handle conn)
    Warp.run port $ mainFunc1 serverH
-}
    CMC.bracket 
        (PS.connectPostgreSQL "dbname='batadase'")
        (\conn -> PS.close conn) -- close connection
        (\conn -> let serverH = ServerHandlers L.simpleLog (DB.Handle conn) in
         Warp.run port $ mainFunc1 serverH)
{-
  -}       

connectToDB :: IO PS.Connection
connectToDB = PS.connectPostgreSQL "dbname='batadase'"

mainFunc1 :: ServerHandlers -> W.Application
mainFunc1 handlers req respond = do
    response <- runServer handlers $ (mainServer req :: ServerIO W.Response)
    respond response

mainServer :: MonadServer m => W.Request -> m W.Response
mainServer req = fmap coerceResponse $ do
    logDebug $ T.pack $ show req
    let eithWhoWhat = requestToAction req
    withTimePrint $ printS $ eithWhoWhat
    logDebug "\n type calculation time is above"
    
    case eithWhoWhat of
        Left err -> handleError err
        Right whowhat -> do
            logDebug "Action type is"
            logDebug $ T.pack $ GP.defaultPretty $ _ww_action whowhat

            val <- withTimePrint (executeAction whowhat
                `CMC.catches` [CMC.Handler Ex.mainErrorHandler,
                               CMC.Handler Ex.defaultMainHandler])
            logDebug "\n execution time is above"
            return val


coerceResponse :: Response -> W.Response
coerceResponse (Response status msg) =
    W.responseLBS status [] $ Ae.encode msg



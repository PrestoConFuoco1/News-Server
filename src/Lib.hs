

module Lib
    ( someFunc
    ) where


import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai as W (Request(..), Response, Application, responseLBS)
import qualified Data.Text as T (pack)

import qualified GenericPretty as GP (defaultPretty)

import Action.RequestToAction (requestToAction)
import Execute (executeAction, handleError)
import Types
import Result

import qualified App.Logger as L (simpleLog, logDebug)
import qualified Database.PostgreSQL.Simple as PS (connectPostgreSQL, Connection, close)
import qualified Data.Aeson as Ae (encode, ToJSON(..))

import qualified Exceptions as Ex (mainErrorHandler, defaultMainHandler)
import qualified Control.Monad.Catch as CMC

import qualified Database.PostgreSQL.Simple.Migration as PSM
import Migrations


import qualified App.Database as D
import qualified App.Database.Postgres as DP

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
        (\conn -> let serverH = DP.connectionToHandle (DP.Connection conn) L.simpleLog in
         Warp.run port $ mainFunc1 serverH)

connectToDB :: IO PS.Connection
connectToDB = PS.connectPostgreSQL "dbname='batadase'"

mainFunc1 :: D.Handle IO -> W.Application
mainFunc1 h req respond = do
    response <- mainServer h req
    respond response

mainServer :: CMC.MonadCatch m => D.Handle m -> W.Request -> m W.Response
mainServer h req = fmap coerceResponse $ do
    D.logDebug h ""
    D.logDebug h "Got request"
    D.logDebug h $ ("Path: " <>) $ T.pack $ show $ W.pathInfo req
    D.logDebug h $ ("Args: " <>) $ T.pack $ show $ W.queryString req
    let eithWhoWhat = requestToAction req

    case eithWhoWhat of
        Left err -> handleError h err
        Right whowhat -> do
            D.logDebug h "Action type is"
            D.logDebug h $ T.pack $ GP.defaultPretty $ _ww_action whowhat

            fmap toResponse (executeAction h whowhat)
                `CMC.catches` [CMC.Handler Ex.mainErrorHandler,
                               CMC.Handler Ex.defaultMainHandler]



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
toResponse (RInvalidForeign ent field value) = bad (invalidForeignMsg field value)
toResponse (RInvalidTag value) = bad (tagNotFoundMsg value)



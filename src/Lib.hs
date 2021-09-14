{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts
#-}


module Lib
    ( someFunc
    ) where



import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as W
import qualified Network.HTTP.Types as NHT
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception
import qualified Data.Aeson as Ae (Value, encode)

import qualified GenericPretty as GP
import GHC.Generics

import RequestToAction
import FromSQL

import qualified Logger as L
import MonadTypes
import qualified Database.PostgreSQL.Simple as PS
import qualified DatabaseHandler as DB
import qualified DBTypes as DBT
import qualified Types as Ty
import qualified Data.Aeson as Ae
import Data.Proxy

port :: Int
port = 5555

someFunc :: IO ()
someFunc = do
    conn <- PS.connectPostgreSQL "dbname='batadase'"
    let serverH = ServerHandlers L.simpleLog (DB.Handle conn)
    Warp.run port $ mainFunc1 serverH

connectToDB :: IO PS.Connection
connectToDB = PS.connectPostgreSQL "dbname='batadase'"

mainFunc1 :: ServerHandlers -> W.Application
mainFunc1 handlers req respond = do
    response <- runServer handlers $ (mainServer req :: ServerIO W.Response)
    respond response

data Response = Response {
    _r_status :: NHT.Status,
    _r_message :: Ae.Value
    }



mainServer :: MonadServer m => W.Request -> m W.Response
mainServer req = do
    logDebug $ T.pack $ show req
    let whowhat@(WhoWhat maybeToken action) = requestToAction req
    logDebug "Action type is"
    logDebug $ T.pack $ GP.defaultPretty action

    val <- executeAction whowhat
    let (Response status msg) = val
    return $ W.responseLBS status [] $ Ae.encode msg

executeAction :: MonadServer m => WhoWhat Action -> m Response
executeAction (WhoWhat y (AGetPosts x)) = getThis postDummy (WhoWhat y x)
executeAction (WhoWhat y (AGetCategories x)) = getThis catDummy (WhoWhat y x)
executeAction (WhoWhat y (AGetAuthors x)) = getThis authorDummy (WhoWhat y x)


--data Permissions = 

getThis :: (FromSQL s, MonadServer m) => s -> WhoWhat (Get s) -> m Response
getThis x (WhoWhat token g) = do
    cat <- f x g
    logDebug $ T.pack $ GP.defaultPretty cat
    let val = Ae.toJSON cat
    return $ Response NHT.ok200 val
  where f :: (FromSQL s, MonadServer m) => s -> Get s -> m [MType s]
        f x g = do
            let qu = selectQuery x g
            debugStr <- uncurry formatQuery qu
            logDebug $ T.pack $ show debugStr
            uncurry query qu




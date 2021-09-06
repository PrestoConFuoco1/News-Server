module Lib
    ( someFunc
    ) where


import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception

import qualified GenericPretty as GP
import GHC.Generics

import RequestToAction

import qualified Logger as L
import Types

port :: Int
port = 5555

someFunc :: IO ()
--someFunc = putStrLn "someFunc"
someFunc = do
    let serverH = ServerHandlers L.simpleLog 0
    run port $ mainFunc1 serverH


mainFunc1 :: ServerHandlers -> Application
mainFunc1 handlers req respond = do
    response <- runServer handlers $ (mainServer req :: ServerIO Response)
 --   respond $ responseLBS status200 [] "Hello, World!\n"
    respond response


mainServer :: MonadServer m => Request -> m Response
mainServer req = do
    logDebug $ T.pack $ show req
    let action = requestToAction req
    logDebug "Action type is"
    logDebug $ T.pack $ GP.defaultPretty action
    return $ responseLBS status200 [] "Hello, World!\n"

















--instance Generic Request
--instance GP.PrettyShow Request
{-
mainFunc :: Application
-- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
mainFunc req respond =
-- bracket_
--    (putStrLn "Allocating scarce resource")
--    (putStrLn "Cleaning up") $ 
         do
        putStrLn $ show req
        let action = requestToAction req
        putStrLn $ show action
        respond $ responseLBS status200 [] "Hello, World!\n"
-}


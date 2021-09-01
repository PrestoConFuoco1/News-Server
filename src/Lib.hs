module Lib
    ( someFunc
    ) where


import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Data.ByteString as B
import Control.Exception

import qualified GenericPretty as GP
import GHC.Generics

port :: Int
port = 5555

someFunc :: IO ()
--someFunc = putStrLn "someFunc"
someFunc = run port mainFunc

--instance Generic Request
--instance GP.PrettyShow Request

mainFunc :: Application
-- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
mainFunc req respond = bracket_
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up") $ do
        putStrLn $ show req
        respond $ responseLBS status200 [] "Hello, World!\n"




import Test.Hspec

import AuthenticationLogic (emulatedLogic)
import Routing

main :: IO ()
main = hspec $ do
    testRouting
    emulatedLogic


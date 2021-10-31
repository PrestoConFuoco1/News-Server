import Test.Hspec

import AuthenticationLogic (emulatedLogic)
import Routing (testRouting)
import CategoryEditCheck (testCategoryEditCheck)

main :: IO ()
main = hspec $ do
    testRouting
    emulatedLogic
    testCategoryEditCheck


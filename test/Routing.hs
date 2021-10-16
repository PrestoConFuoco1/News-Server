module Routing where

import Test.Hspec

import Action.RequestToAction
import Types

testRouting :: Spec
testRouting = do
    describe "test routing" $
        it "correctly handles authentication" $
        (requestToAction1 ["auth"] [("login", Just "login1"), ("pass_hash", Just "password1")])
            `shouldBe` Right (WhoWhat Nothing $ AAuth $ Authenticate {
                _au_login = "login1"
                , _au_passHash = "password1"
                })

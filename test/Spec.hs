import Test.Hspec

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = hspec $ do
  describe "Prelude" $ do
    describe "read" $ do
        it "can parse integers" $ do
            read "10" `shouldBe` (10::Int)
    describe "read" $ do
        it "returns the first element of a list" $ do
            head [23..] `shouldBe` 23

module CategoryEditCheck where

import Test.Hspec

import qualified Types as Y
import Execute.Utils
import Data.Maybe (isNothing)

testNewParentCat = Y.Category {
    Y.cCategoryId = 3
    , Y.categoryDescription = "category_level3"
    , Y.catParentCategory = Just Y.Category {
        Y.cCategoryId = 2
        , Y.categoryDescription = "category_level2"
        , Y.catParentCategory = Just Y.Category {
            Y.cCategoryId = 1
            , Y.categoryDescription = "category_level1"
            , Y.catParentCategory = Nothing
            }
        }
    }

isConstraintViolation :: Y.ModifyError -> Bool
isConstraintViolation (Y.MConstraintViolated (Y.ConstraintViolation {})) = True
isConstraintViolation _ = False

testCategoryEditCheck :: Spec
testCategoryEditCheck = describe "test category editing checks" $ do
    it "should reject edit tries that form a cycle" $
        fmap isConstraintViolation (checkCategoryCycles 2 testNewParentCat) `shouldBe` Just True
    it "should pass if no cycles are formed" $
        isNothing (checkCategoryCycles 4 testNewParentCat) `shouldBe` True

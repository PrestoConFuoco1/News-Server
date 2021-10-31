module CategoryEditCheck where

import Test.Hspec

import qualified Types as Y
import Execute.Utils
import Data.Maybe (isNothing)

testNewParentCat = Y.Category {
    Y._cat_categoryId = 3
    , Y._cat_description = "category_level3"
    , Y._cat_parentCategory = Just Y.Category {
        Y._cat_categoryId = 2
        , Y._cat_description = "category_level2"
        , Y._cat_parentCategory = Just Y.Category {
            Y._cat_categoryId = 1
            , Y._cat_description = "category_level1"
            , Y._cat_parentCategory = Nothing
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

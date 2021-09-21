{-# LANGUAGE DeriveAnyClass #-}
module Action.Category where

import Action.Utils

import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
import Data.Void


type ActionCategory = CRUD CreateCategory GetCategories EditCategory DeleteCategory

type CatId = Int

instance GP.PrettyShow GetCategories where
    prettyShow = GP.LStr . show


data GetCategories = GetCategories
    deriving (Show, Generic)

data CreateCategory = CreateCategory {
    _cc_catName :: T.Text,
    _cc_parentCat :: CatId
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data EditCategory = EditCategory {
    _ec_catId :: CatId,
    _ec_catName :: Maybe T.Text,
    _ec_parentId :: Maybe CatId
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data DeleteCategory = DeleteCategory {
    _dc_catId :: CatId
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)



createCatsToAction :: Router CreateCategory
createCatsToAction = do
    name <- requireField (validator notEmpty . readText) "name"
    parentId <- requireField readInt "parent_id"
    return $ CreateCategory name parentId

editCatsToAction :: Router EditCategory
editCatsToAction = do
    id <- requireField readInt "category_id"
    parentId <- optional readInt "parent_id"
    name <- optional (validator notEmpty . readText) "name"
    return $ EditCategory id name parentId

deleteCatsToAction :: Router DeleteCategory
deleteCatsToAction = do
    id <- requireField readInt "category_id"
    return $ DeleteCategory id



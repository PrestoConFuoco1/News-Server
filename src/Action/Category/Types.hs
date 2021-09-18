{-# LANGUAGE DeriveAnyClass #-}
module Action.Category.Types where


import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
{-
data ActionCategory = CGet GetCategories
                 | CCreate CreateCategory
                 | CEdit EditCategory
                 | CDelete DeleteCategory
-}
type ActionCategory = CRUD CreateCategory GetCategories EditCategory DeleteCategory

type CatId = Int

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
    } deriving (Show, Generic, GP.PrettyShow)

data DeleteCategory = DeleteCategory {
    _dc_catId :: CatId
    } deriving (Show, Generic, GP.PrettyShow)

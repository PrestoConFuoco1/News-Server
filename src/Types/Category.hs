{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Category
    ( CategoryId
    , GetCategories(..)
    , CreateCategory(..)
    , EditCategory(..)
    , DeleteCategory(..)
    , ActionCategory
    ) where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Types.Common (CRUD, Paginated)

type CategoryId = Int

type ActionCategory
     = CRUD CreateCategory (Paginated GetCategories) EditCategory DeleteCategory

newtype GetCategories =
    GetCategories
        { gcCategoryId :: Maybe CategoryId
        }
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable GetCategories

data CreateCategory =
    CreateCategory
        { ccCategoryName :: Text.Text
        , ccParentCategory :: CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data EditCategory =
    EditCategory
        { ecCategoryId :: CategoryId
        , ecCategoryName :: Maybe Text.Text
        , ecParentId :: Maybe CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

newtype DeleteCategory =
    DeleteCategory
        { dcCategoryId :: CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)


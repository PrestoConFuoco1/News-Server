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
        { _gc_catId :: Maybe CategoryId
        }
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable GetCategories

data CreateCategory =
    CreateCategory
        { _cc_catName :: Text.Text
        , _cc_parentCat :: CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data EditCategory =
    EditCategory
        { _ec_catId :: CategoryId
        , _ec_catName :: Maybe Text.Text
        , _ec_parentId :: Maybe CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

newtype DeleteCategory =
    DeleteCategory
        { _dc_catId :: CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)


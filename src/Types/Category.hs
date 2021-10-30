{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Category where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Types.Common

type CategoryId = Int

type ActionCategory
     = CRUD CreateCategory (Paginated GetCategories) EditCategory DeleteCategory

data GetCategories =
    GetCategories
  deriving (Show, Eq, Generic)
    deriving GP.PrettyShow via GP.Showable GetCategories

data CreateCategory =
    CreateCategory
        { _cc_catName :: T.Text
        , _cc_parentCat :: CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data EditCategory =
    EditCategory
        { _ec_catId :: CategoryId
        , _ec_catName :: Maybe T.Text
        , _ec_parentId :: Maybe CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

newtype DeleteCategory =
    DeleteCategory
        { _dc_catId :: CategoryId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)


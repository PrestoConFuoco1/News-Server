{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Tags
    ( TagId
    , EditTag(..)
    , GetTags(..)
    , DeleteTag(..)
    , CreateTag(..)
    , ActionTags
    ) where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Types.Common (CRUD, Paginated)

type TagId = Int

type ActionTags = CRUD CreateTag (Paginated GetTags) EditTag DeleteTag

data GetTags =
    GetTags
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable GetTags

newtype CreateTag =
    CreateTag
        { ctTagName :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data EditTag =
    EditTag
        { etTagId :: TagId
        , etTagName :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

newtype DeleteTag =
    DeleteTag
        { dtTagId :: TagId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Authors
    ( AuthorId
    , EditAuthor(..)
    , GetAuthors(..)
    , DeleteAuthor(..)
    , CreateAuthor(..)
    , ActionAuthors
    ) where

import qualified Data.Text as Text
import GHC.Generics
import qualified GenericPretty as GP
import Types.Common (CRUD, Paginated)
import Types.Users (UserId)

type AuthorId = Int

type ActionAuthors
     = CRUD CreateAuthor (Paginated GetAuthors) EditAuthor DeleteAuthor

newtype GetAuthors =
    GetAuthors
        { gaUserId :: Maybe UserId
        }
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable GetAuthors

data CreateAuthor =
    CreateAuthor
        { caUserId :: UserId
        , caDescription :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

newtype DeleteAuthor =
    DeleteAuthor
        { daAuthorId :: AuthorId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

data EditAuthor =
    EditAuthor
        { eaAuthorId :: AuthorId
        , eaDescription :: Maybe Text.Text
        , eaUserId :: Maybe UserId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)


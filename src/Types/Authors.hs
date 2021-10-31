{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Authors (
AuthorId,
EditAuthor(..),
GetAuthors(..),
DeleteAuthor(..),
CreateAuthor(..),
ActionAuthors
) where

import qualified Data.Text as T
import GHC.Generics
import qualified GenericPretty as GP
import Types.Common (CRUD, Paginated)
import Types.Users (UserId)

type AuthorId = Int

type ActionAuthors
     = CRUD CreateAuthor (Paginated GetAuthors) EditAuthor DeleteAuthor

newtype GetAuthors =
    GetAuthors
        { _ga_userId :: Maybe UserId
        }
  deriving (Show, Eq, Generic)
    deriving GP.PrettyShow via GP.Showable GetAuthors

data CreateAuthor =
    CreateAuthor
        { _ca_userId :: UserId
        , _ca_description :: T.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

newtype DeleteAuthor =
    DeleteAuthor
        { _da_authorId :: AuthorId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

data EditAuthor =
    EditAuthor
        { _ea_authorId :: AuthorId
        , _ea_description :: Maybe T.Text
        , _ea_userId :: Maybe UserId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)


{-# LANGUAGE DeriveAnyClass #-}

module Types.Authors where

import qualified Data.Text as T
import GHC.Generics
import qualified GenericPretty as GP
import Types.Common
import Types.Users

type AuthorId = Int

type ActionAuthors
    = CRUD CreateAuthor (Paginated GetAuthors) EditAuthor DeleteAuthor

instance GP.PrettyShow GetAuthors where
   prettyShow = GP.LStr . show

newtype GetAuthors =
   GetAuthors
      { _ga_userId :: Maybe UserId
      }
   deriving (Show, Eq, Generic)

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

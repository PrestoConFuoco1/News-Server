{-# LANGUAGE DeriveAnyClass #-}
module Action.Authors.Types where

import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
import Data.Void
import qualified Data.Text as T

--type Author = T.Text

type ActionAuthors = CRUD CreateAuthor GetAuthors EditAuthor DeleteAuthor


data GetAuthors = GetAuthors {
    _ga_userId :: Maybe Int
    } deriving (Show, Generic)

data CreateAuthor = CreateAuthor {
    _ca_userId :: Int,
    _ca_description :: T.Text
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data DeleteAuthor = DeleteAuthor {
    _da_authorId :: Int
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data EditAuthor = EditAuthor {
    _ea_authorId :: Int,
    _ea_description :: Maybe T.Text,
    _ea_userId :: Maybe Int
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)


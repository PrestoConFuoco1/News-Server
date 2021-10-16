{-# LANGUAGE DeriveAnyClass #-}
module Types.Authors where


import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import Types.Common
import Data.Void
import qualified Data.Text as T
import Types.Users

type AuthorId = Int
type ActionAuthors = CRUD CreateAuthor (Paginated GetAuthors) EditAuthor DeleteAuthor
instance GP.PrettyShow GetAuthors where
    prettyShow = GP.LStr . show



data GetAuthors = GetAuthors {
    _ga_userId :: Maybe UserId
    } deriving (Show, Eq, Generic)

data CreateAuthor = CreateAuthor {
    _ca_userId :: UserId,
    _ca_description :: T.Text
    } deriving (Show, Eq, Generic, GP.PrettyShow) --, PS.ToRow)

data DeleteAuthor = DeleteAuthor {
    _da_authorId :: AuthorId
    } deriving (Show, Eq, Generic, GP.PrettyShow) --, PS.ToRow)

data EditAuthor = EditAuthor {
    _ea_authorId :: AuthorId,
    _ea_description :: Maybe T.Text,
    _ea_userId :: Maybe UserId
    } deriving (Show, Eq, Generic, GP.PrettyShow) --, PS.ToRow)



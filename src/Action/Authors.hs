{-# LANGUAGE DeriveAnyClass #-}
module Action.Authors where


import Action.Utils
import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
import Data.Void
import qualified Data.Text as T


type ActionAuthors = CRUD CreateAuthor GetAuthors EditAuthor DeleteAuthor
instance GP.PrettyShow GetAuthors where
    prettyShow = GP.LStr . show



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


createAuthorToAction :: Router CreateAuthor
createAuthorToAction = do
    userId <- requireField readInt "user_id"
    description <- requireField (validator notEmpty . readText) "description"
    return $ CreateAuthor userId description


deleteAuthorToAction :: Router DeleteAuthor
deleteAuthorToAction = do
    authorId <- requireField readInt "author_id"
    return $ DeleteAuthor authorId


editAuthorToAction :: Router EditAuthor
editAuthorToAction = do
    
    authorId <- requireField readInt "author_id"
    userId <- optional readInt "user_id"
    description <- optional (validator notEmpty . readText) "description"
    return $ EditAuthor authorId description userId


{-
-}


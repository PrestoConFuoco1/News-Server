{-# LANGUAGE DeriveAnyClass #-}
module Action.Authors where


import Action.Utils
import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
import Data.Void
import qualified Data.Text as T
import Types

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


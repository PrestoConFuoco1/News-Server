{-# LANGUAGE DeriveAnyClass #-}
module Action.Authors where


import Action.Utils
import Action.Common
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


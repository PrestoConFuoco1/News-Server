module Action.Authors where

import Action.Common (Router)
import Action.Utils
import Types

createAuthorToAction :: Router CreateAuthor
createAuthorToAction = do
   userId <- requireField readInt "user_id"
   description <-
      requireField
         (validator notEmpty . readText)
         "description"
   pure $ CreateAuthor userId description

deleteAuthorToAction :: Router DeleteAuthor
deleteAuthorToAction = do
   authorId <- requireField readInt "author_id"
   pure $ DeleteAuthor authorId

editAuthorToAction :: Router EditAuthor
editAuthorToAction = do
   authorId <- requireField readInt "author_id"
   userId <- optional readInt "user_id"
   description <-
      optional (validator notEmpty . readText) "description"
   pure $ EditAuthor authorId description userId
{-
-}

module Action.Authors where

import Action.Common (Router)
import qualified Action.Utils as AU
import Types

createAuthorToAction :: Router CreateAuthor
createAuthorToAction = do
   userId <- AU.requireField AU.readInt "user_id"
   description <-
      AU.requireField
         (AU.validator AU.notEmpty . AU.readText)
         "description"
   pure $ CreateAuthor userId description

deleteAuthorToAction :: Router DeleteAuthor
deleteAuthorToAction = do
   authorId <- AU.requireField AU.readInt "author_id"
   pure $ DeleteAuthor authorId

editAuthorToAction :: Router EditAuthor
editAuthorToAction = do
   authorId <- AU.requireField AU.readInt "author_id"
   userId <- AU.optional AU.readInt "user_id"
   description <-
      AU.optional (AU.validator AU.notEmpty . AU.readText) "description"
   pure $ EditAuthor authorId description userId
{-
-}

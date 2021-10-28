module Action.Authors where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as Y

createAuthorToAction :: Router Y.CreateAuthor
createAuthorToAction = do
   userId <- AU.requireField AU.readInt "user_id"
   description <-
      AU.requireField
         (AU.validator AU.notEmpty . AU.readText)
         "description"
   pure $ Y.CreateAuthor userId description

deleteAuthorToAction :: Router Y.DeleteAuthor
deleteAuthorToAction = do
   authorId <- AU.requireField AU.readInt "author_id"
   pure $ Y.DeleteAuthor authorId

editAuthorToAction :: Router Y.EditAuthor
editAuthorToAction = do
   authorId <- AU.requireField AU.readInt "author_id"
   userId <- AU.optional AU.readInt "user_id"
   description <-
      AU.optional (AU.validator AU.notEmpty . AU.readText) "description"
   pure $ Y.EditAuthor authorId description userId
{-
-}

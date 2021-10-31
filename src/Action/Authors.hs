module Action.Authors
    ( createAuthorToAction
    , deleteAuthorToAction
    , editAuthorToAction
    ) where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as T

createAuthorToAction :: Router T.CreateAuthor
createAuthorToAction = do
    userId <- AU.requireField AU.readInt "user_id"
    description <-
        AU.requireField
            (AU.validator AU.notEmpty . AU.readText)
            "description"
    pure $ T.CreateAuthor userId description

deleteAuthorToAction :: Router T.DeleteAuthor
deleteAuthorToAction = do
    authorId <- AU.requireField AU.readInt "author_id"
    pure $ T.DeleteAuthor authorId

editAuthorToAction :: Router T.EditAuthor
editAuthorToAction = do
    authorId <- AU.requireField AU.readInt "author_id"
    userId <- AU.optional AU.readInt "user_id"
    description <-
        AU.optional
            (AU.validator AU.notEmpty . AU.readText)
            "description"
    pure $ T.EditAuthor authorId description userId
{-
-}

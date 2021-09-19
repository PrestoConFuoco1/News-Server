module Action.Authors.Authors where

import Action.Common
import qualified Data.Text as T
import Action.Authors.Types
import Action.Utils

requestToActionAuthors :: [T.Text] -> Query -> Either ActionError ActionAuthors
requestToActionAuthors path hash = case path of
  (x:xs)
    | x == "get" -> Right $ Read GetAuthors
    | x == "create" -> fmap Create $ createAuthorToAction hash
    | x == "delete" -> fmap Delete $ deleteAuthorToAction hash
    | x == "edit" -> fmap Update $ editAuthorToAction hash
  [] -> Left EInvalidEndpoint


createAuthorToAction :: Query -> Either ActionError CreateAuthor
createAuthorToAction hash = do
    userId <- requireField (requireInt hash) "user_id"
    description <- requireField (requireText hash) "description"
    return $ CreateAuthor userId description



deleteAuthorToAction :: Query -> Either ActionError DeleteAuthor
deleteAuthorToAction hash = do
    authorId <- requireField (requireInt hash) "author_id"
    return $ DeleteAuthor authorId


editAuthorToAction :: Query -> Either ActionError EditAuthor
editAuthorToAction hash = do
    authorId <- requireField (requireInt hash) "author_id"
    let userId = requireInt hash "user_id"
        description = requireText hash "description"

    return $ EditAuthor authorId description userId



module Action.Category.Category where

import Action.Category.Types
import qualified Data.Text as T
import Action.Common
import Action.Utils

requestToActionCats :: [T.Text] -> Query -> Either ActionError ActionCategory
requestToActionCats path hash = case path of
  x:[]
    | x == "get" -> Right $ Read GetCategories
    | x == "create" -> fmap Create $ createCatsToAction hash
    | x == "edit" -> fmap Update $ editCatsToAction hash
    | x == "delete" -> fmap Delete $ deleteCatsToAction hash
  _ -> Left EInvalidEndpoint

createCatsToAction :: Query -> Either ActionError CreateCategory
createCatsToAction hash = do
    name <- validateRequired notEmpty (requireText hash) "name"
    parentId <- requireField (requireInt hash) "parent_id"
    return $ CreateCategory name parentId

editCatsToAction :: Query -> Either ActionError EditCategory
editCatsToAction hash = do
    id <- requireField (requireInt hash) "category_id"
--    let name = requireText hash "name"
    let parentId = requireInt hash "parent_id"
    name <- validateOptional notEmpty (requireText hash) "name"
    return $ EditCategory id name parentId

deleteCatsToAction :: Query -> Either ActionError DeleteCategory
deleteCatsToAction hash = do
    id <- requireField (requireInt hash) "category_id"
    return $ DeleteCategory id



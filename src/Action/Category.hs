module Action.Category
    ( createCatsToAction
    , editCatsToAction
    , deleteCatsToAction
    ) where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as T

createCatsToAction :: Router T.CreateCategory
createCatsToAction = do
    name <-
        AU.requireField
            (AU.validator AU.notEmpty . AU.readText)
            "name"
    parentId <- AU.requireField AU.readInt "parent_id"
    pure $ T.CreateCategory name parentId

editCatsToAction :: Router T.EditCategory
editCatsToAction = do
    cid <- AU.requireField AU.readInt "category_id"
    parentId <- AU.optional AU.readInt "parent_id"
    name <-
        AU.optional (AU.validator AU.notEmpty . AU.readText) "name"
    pure $ T.EditCategory cid name parentId

deleteCatsToAction :: Router T.DeleteCategory
deleteCatsToAction = do
    cid <- AU.requireField AU.readInt "category_id"
    pure $ T.DeleteCategory cid

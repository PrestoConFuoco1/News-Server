module Action.Category
    ( createCatsToAction
    , editCatsToAction
    , deleteCatsToAction
    ) where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as Y

createCatsToAction :: Router Y.CreateCategory
createCatsToAction = do
    name <-
        AU.requireField
            (AU.validator AU.notEmpty . AU.readText)
            "name"
    parentId <- AU.requireField AU.readInt "parent_id"
    pure $ Y.CreateCategory name parentId

editCatsToAction :: Router Y.EditCategory
editCatsToAction = do
    cid <- AU.requireField AU.readInt "category_id"
    parentId <- AU.optional AU.readInt "parent_id"
    name <-
        AU.optional (AU.validator AU.notEmpty . AU.readText) "name"
    pure $ Y.EditCategory cid name parentId

deleteCatsToAction :: Router Y.DeleteCategory
deleteCatsToAction = do
    cid <- AU.requireField AU.readInt "category_id"
    pure $ Y.DeleteCategory cid

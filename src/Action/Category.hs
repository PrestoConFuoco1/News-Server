module Action.Category where

import Action.Common (Router)
import qualified Action.Utils as AU
import Types

createCatsToAction :: Router CreateCategory
createCatsToAction = do
   name <-
      AU.requireField (AU.validator AU.notEmpty . AU.readText) "name"
   parentId <- AU.requireField AU.readInt "parent_id"
   pure $ CreateCategory name parentId

editCatsToAction :: Router EditCategory
editCatsToAction = do
   cid <- AU.requireField AU.readInt "category_id"
   parentId <- AU.optional AU.readInt "parent_id"
   name <- AU.optional (AU.validator AU.notEmpty . AU.readText) "name"
   pure $ EditCategory cid name parentId

deleteCatsToAction :: Router DeleteCategory
deleteCatsToAction = do
   cid <- AU.requireField AU.readInt "category_id"
   pure $ DeleteCategory cid

module Action.Category where

import Action.Common (Router)
import Action.Utils
import Types

createCatsToAction :: Router CreateCategory
createCatsToAction = do
   name <-
      requireField (validator notEmpty . readText) "name"
   parentId <- requireField readInt "parent_id"
   pure $ CreateCategory name parentId

editCatsToAction :: Router EditCategory
editCatsToAction = do
   cid <- requireField readInt "category_id"
   parentId <- optional readInt "parent_id"
   name <- optional (validator notEmpty . readText) "name"
   pure $ EditCategory cid name parentId

deleteCatsToAction :: Router DeleteCategory
deleteCatsToAction = do
   cid <- requireField readInt "category_id"
   pure $ DeleteCategory cid

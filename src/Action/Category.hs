{-# LANGUAGE DeriveAnyClass #-}
module Action.Category where

import Action.Utils
import Action.Common
import Types


createCatsToAction :: Router CreateCategory
createCatsToAction = do
    name <- requireField (validator notEmpty . readText) "name"
    parentId <- requireField readInt "parent_id"
    return $ CreateCategory name parentId

editCatsToAction :: Router EditCategory
editCatsToAction = do
    id <- requireField readInt "category_id"
    parentId <- optional readInt "parent_id"
    name <- optional (validator notEmpty . readText) "name"
    return $ EditCategory id name parentId

deleteCatsToAction :: Router DeleteCategory
deleteCatsToAction = do
    id <- requireField readInt "category_id"
    return $ DeleteCategory id



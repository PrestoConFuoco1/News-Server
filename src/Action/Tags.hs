{-# LANGUAGE DeriveAnyClass #-}
module Action.Tags where

import qualified Data.Text as T
import Action.Utils

import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import Action.Common
import Types



createTagToAction :: Router CreateTag
createTagToAction = do
    name <- requireField validateNotEmpty "name"
    return $ CreateTag name

editTagToAction :: Router EditTag
editTagToAction = do
    id <- requireField readInt "tag_id"
    name <- requireField validateNotEmpty "name"
    return $ EditTag id name

deleteTagToAction :: Router DeleteTag
deleteTagToAction = do
    id <- requireField readInt "tag_id"
    return $ DeleteTag id








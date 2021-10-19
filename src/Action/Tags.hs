module Action.Tags where

import Action.Utils
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








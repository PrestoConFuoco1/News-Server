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
    tid <- requireField readInt "tag_id"
    name <- requireField validateNotEmpty "name"
    return $ EditTag tid name

deleteTagToAction :: Router DeleteTag
deleteTagToAction = do
    tid <- requireField readInt "tag_id"
    return $ DeleteTag tid








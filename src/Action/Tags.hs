module Action.Tags where

import Action.Common
import Action.Utils
import Types

createTagToAction :: Router CreateTag
createTagToAction = do
   name <- requireField validateNotEmpty "name"
   pure $ CreateTag name

editTagToAction :: Router EditTag
editTagToAction = do
   tid <- requireField readInt "tag_id"
   name <- requireField validateNotEmpty "name"
   pure $ EditTag tid name

deleteTagToAction :: Router DeleteTag
deleteTagToAction = do
   tid <- requireField readInt "tag_id"
   pure $ DeleteTag tid

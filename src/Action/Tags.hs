module Action.Tags where

import Action.Common (Router)
import qualified Action.Utils as AU
import Types

createTagToAction :: Router CreateTag
createTagToAction = do
   name <- AU.requireField AU.validateNotEmpty "name"
   pure $ CreateTag name

editTagToAction :: Router EditTag
editTagToAction = do
   tid <- AU.requireField AU.readInt "tag_id"
   name <- AU.requireField AU.validateNotEmpty "name"
   pure $ EditTag tid name

deleteTagToAction :: Router DeleteTag
deleteTagToAction = do
   tid <- AU.requireField AU.readInt "tag_id"
   pure $ DeleteTag tid

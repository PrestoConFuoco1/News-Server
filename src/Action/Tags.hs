module Action.Tags
    ( createTagToAction
    , editTagToAction
    , deleteTagToAction
    ) where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as Y

createTagToAction :: Router Y.CreateTag
createTagToAction = do
    name <- AU.requireField AU.validateNotEmpty "name"
    pure $ Y.CreateTag name

editTagToAction :: Router Y.EditTag
editTagToAction = do
    tid <- AU.requireField AU.readInt "tag_id"
    name <- AU.requireField AU.validateNotEmpty "name"
    pure $ Y.EditTag tid name

deleteTagToAction :: Router Y.DeleteTag
deleteTagToAction = do
    tid <- AU.requireField AU.readInt "tag_id"
    pure $ Y.DeleteTag tid

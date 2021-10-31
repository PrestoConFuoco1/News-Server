module Action.Tags
    ( createTagToAction
    , editTagToAction
    , deleteTagToAction
    ) where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as T

createTagToAction :: Router T.CreateTag
createTagToAction = do
    name <- AU.requireField AU.validateNotEmpty "name"
    pure $ T.CreateTag name

editTagToAction :: Router T.EditTag
editTagToAction = do
    tid <- AU.requireField AU.readInt "tag_id"
    name <- AU.requireField AU.validateNotEmpty "name"
    pure $ T.EditTag tid name

deleteTagToAction :: Router T.DeleteTag
deleteTagToAction = do
    tid <- AU.requireField AU.readInt "tag_id"
    pure $ T.DeleteTag tid

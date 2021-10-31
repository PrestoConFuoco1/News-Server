module Action.Comments
    ( getCommentsToAction
    , createCommentsToAction
    , deleteCommentsToAction
    ) where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as T

getCommentsToAction :: Router T.GetComments
getCommentsToAction = do
    postId <- AU.requireField AU.readInt "post_id"
    pure $ T.GetComments postId

createCommentsToAction :: Router T.CreateComment
createCommentsToAction = do
    postId <- AU.requireField AU.readInt "post_id"
    content <-
        AU.requireField
            (AU.validator AU.notEmpty . AU.readText)
            "content"
    pure $ T.CreateComment postId content

deleteCommentsToAction :: Router T.DeleteComment
deleteCommentsToAction = do
    commentId <- AU.requireField AU.readInt "comment_id"
    pure $ T.DeleteComment commentId

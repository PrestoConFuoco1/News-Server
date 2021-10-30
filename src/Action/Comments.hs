module Action.Comments (
getCommentsToAction,
createCommentsToAction,
deleteCommentsToAction
) where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as Y

getCommentsToAction :: Router Y.GetComments
getCommentsToAction = do
    postId <- AU.requireField AU.readInt "post_id"
    pure $ Y.GetComments postId

createCommentsToAction :: Router Y.CreateComment
createCommentsToAction = do
    postId <- AU.requireField AU.readInt "post_id"
    content <-
        AU.requireField
            (AU.validator AU.notEmpty . AU.readText)
            "content"
    pure $ Y.CreateComment postId content

deleteCommentsToAction :: Router Y.DeleteComment
deleteCommentsToAction = do
    commentId <- AU.requireField AU.readInt "comment_id"
    pure $ Y.DeleteComment commentId

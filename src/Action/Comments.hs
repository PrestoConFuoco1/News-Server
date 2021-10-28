module Action.Comments where

import Action.Common (Router)
import qualified Action.Utils as AU
import Types

getCommentsToAction :: Router GetComments
getCommentsToAction = do
   postId <- AU.requireField AU.readInt "post_id"
   pure $ GetComments postId

createCommentsToAction :: Router CreateComment
createCommentsToAction = do
   postId <- AU.requireField AU.readInt "post_id"
   content <-
      AU.requireField (AU.validator AU.notEmpty . AU.readText) "content"
   pure $ CreateComment postId content

deleteCommentsToAction :: Router DeleteComment
deleteCommentsToAction = do
   commentId <- AU.requireField AU.readInt "comment_id"
   pure $ DeleteComment commentId

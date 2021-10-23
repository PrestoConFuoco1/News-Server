module Action.Comments where

import Action.Common
import Action.Utils
import Types

getCommentsToAction :: Router GetComments
getCommentsToAction = do
   postId <- requireField readInt "post_id"
   pure $ GetComments postId

createCommentsToAction :: Router CreateComment
createCommentsToAction = do
   postId <- requireField readInt "post_id"
   content <-
      requireField (validator notEmpty . readText) "content"
   pure $ CreateComment postId content

deleteCommentsToAction :: Router DeleteComment
deleteCommentsToAction = do
   commentId <- requireField readInt "comment_id"
   pure $ DeleteComment commentId

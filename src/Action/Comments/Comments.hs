module Action.Comments.Comments where

import Action.Comments.Types
import qualified Data.Text as T
import Action.Common
import Action.Utils



requestToActionComments :: [T.Text] -> Query -> Either ActionError ActionComments
requestToActionComments path hash = case path of
    (x:xs)
      | x == "get" -> fmap Read $ getCommentsToAction hash
      | x == "create" -> fmap Create $ createCommentsToAction hash
      | x == "delete" -> fmap Delete $ deleteCommentsToAction hash


getCommentsToAction :: Query -> Either ActionError GetComments
getCommentsToAction hash = do
    postId <- requireField (requireInt hash) "post_id"
    return $ GetComments postId


createCommentsToAction :: Query -> Either ActionError CreateComment
createCommentsToAction hash = do
    postId <- requireField (requireInt hash) "post_id"
    content <- requireField (requireText hash) "content"
    return $ CreateComment postId content


deleteCommentsToAction :: Query -> Either ActionError DeleteComment
deleteCommentsToAction hash = do
    commentId <- requireField (requireInt hash) "comment_id"
    return $ DeleteComment commentId



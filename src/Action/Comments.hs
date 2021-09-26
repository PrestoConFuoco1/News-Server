{-# LANGUAGE DeriveAnyClass #-}
module Action.Comments where

import Action.Utils
import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
import Data.Void
import Types


getCommentsToAction :: Router GetComments
getCommentsToAction = do
    postId <- requireField readInt "post_id"
    return $ GetComments postId


createCommentsToAction :: Router CreateComment
createCommentsToAction  = do
    postId <- requireField readInt "post_id"
    content <- requireField (validator notEmpty . readText) "content"
    return $ CreateComment postId content


deleteCommentsToAction :: Router DeleteComment
deleteCommentsToAction = do
    commentId <- requireField readInt "comment_id"
    return $ DeleteComment commentId



{-# LANGUAGE DeriveAnyClass #-}
module Action.Comments where

import Action.Utils
import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
import Data.Void

type ActionComments = CRUD CreateComment (Paginated GetComments) Void DeleteComment

data GetComments = GetComments {
    _gc_postId :: Int
    } deriving (Show, Generic, GP.PrettyShow)

data CreateComment = CreateComment {
    _ccom_postId :: Int,
    _ccom_content :: T.Text
    -- user id will be known from auth token
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data DeleteComment = DeleteComment {
    _dc_commentId :: Int
    -- user id will be known from auth token
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)



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


{-# LANGUAGE TypeFamilies #-}

module Database.Delete where

import Data.Maybe (fromMaybe)
import qualified Database.PostgreSQL.Simple as PS
import Database.SqlValue
import Types

class DeleteSQL s where
   type Del s :: *
   deleteQuery :: s -> Del s -> (PS.Query, [SqlValue])
   dName :: s -> Entity


data DTag = DTag

instance DeleteSQL DTag where
   type Del DTag = DeleteTag
   deleteQuery _ dt =
      ( "DELETE FROM news.tag WHERE tag_id = ? RETURNING tag_id"
      , [SqlValue $ _dt_tagId dt])
   dName _ = ETag


data DCat = DCat

instance DeleteSQL DCat where
   type Del DCat = DeleteCategory
   deleteQuery _ dc =
      ( "DELETE FROM news.category WHERE category_id = ? RETURNING category_id"
      , [SqlValue $ _dc_catId dc])
   dName _ = ECategory


data DAuthor = DAuthor

instance DeleteSQL DAuthor where
   type Del DAuthor = DeleteAuthor
   deleteQuery _ da =
      ( "DELETE FROM news.author WHERE author_id = ? RETURNING author_id"
      , [SqlValue $ _da_authorId da])
   dName _ = EAuthor


data DUser = DUser

instance DeleteSQL DUser where
   type Del DUser = DeleteUser
   deleteQuery _ du =
      ( "DELETE FROM news.users WHERE user_id = ? RETURNING user_id"
      , [SqlValue $ _du_userId du])
   dName _ = EUser


data DComment = DComment

isAdmin :: User -> Bool
isAdmin u = fromMaybe False $ _u_admin u

instance DeleteSQL DComment where
   type Del DComment = WithUser DeleteComment
   deleteQuery _ (WithUser u dc) =
      let str =
             " DELETE FROM news.comment WHERE comment_id = ? "
          userWhere = " AND user_id = ? "
          returning = " RETURNING comment_id"
          userParam = SqlValue $ _u_id u
          commentParam = SqlValue $ _dc_commentId dc
       in if isAdmin u
             then (str <> returning, [commentParam])
             else ( str <> userWhere <> returning
                  , [commentParam, userParam])
   dName _ = EComment


data DDraft = DDraft

instance DeleteSQL DDraft where
   type Del DDraft = WithAuthor DeleteDraft
   deleteQuery _ (WithAuthor a (DeleteDraft d)) =
      ( "\
\ DELETE FROM news.draft \
\ WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
      , [SqlValue d, SqlValue a])
   dName _ = EDraft


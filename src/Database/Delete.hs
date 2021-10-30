{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Delete (
DeleteSQL(..)
) where

import Data.Maybe (fromMaybe)
import qualified Database.PostgreSQL.Simple as PS
import Database.SqlValue
import qualified Types as Y

class DeleteSQL a where
    deleteQuery :: a -> (PS.Query, [SqlValue])
    dName :: Y.Entity

instance DeleteSQL Y.DeleteTag where
    deleteQuery dt =
        ( "DELETE FROM news.tag WHERE tag_id = ? RETURNING tag_id"
        , [SqlValue $ Y._dt_tagId dt])
    dName = Y.ETag

instance DeleteSQL Y.DeleteCategory where
    deleteQuery dc =
        ( "DELETE FROM news.category WHERE category_id = ? RETURNING category_id"
        , [SqlValue $ Y._dc_catId dc])
    dName = Y.ECategory

instance DeleteSQL Y.DeleteAuthor where
    deleteQuery da =
        ( "DELETE FROM news.author WHERE author_id = ? RETURNING author_id"
        , [SqlValue $ Y._da_authorId da])
    dName = Y.EAuthor

instance DeleteSQL Y.DeleteUser where
    deleteQuery du =
        ( "DELETE FROM news.users WHERE user_id = ? RETURNING user_id"
        , [SqlValue $ Y._du_userId du])
    dName = Y.EUser

isAdmin :: Y.User -> Bool
isAdmin u = fromMaybe False $ Y._u_admin u

instance DeleteSQL (Y.WithUser Y.DeleteComment) where
    deleteQuery (Y.WithUser u dc) =
        let str = " DELETE FROM news.comment WHERE comment_id = ? "
            userWhere = " AND user_id = ? "
            returning = " RETURNING comment_id"
            userParam = SqlValue $ Y._u_id u
            commentParam = SqlValue $ Y._dc_commentId dc
         in if isAdmin u
                then (str <> returning, [commentParam])
                else ( str <> userWhere <> returning
                     , [commentParam, userParam])
    dName = Y.EComment

instance DeleteSQL (Y.WithAuthor Y.DeleteDraft) where
    deleteQuery (Y.WithAuthor a (Y.DeleteDraft d)) =
        ( "\
\ DELETE FROM news.draft \
\ WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
        , [SqlValue d, SqlValue a])
    dName = Y.EDraft

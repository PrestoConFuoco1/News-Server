{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Delete
    ( DeleteSQL(..)
    ) where

import Data.Maybe (fromMaybe)
import qualified Database.PostgreSQL.Simple as PS
import Database.SqlValue
import qualified Types as T

class DeleteSQL a where
    deleteQuery :: a -> (PS.Query, [SqlValue])
    dName :: T.Entity

instance DeleteSQL T.DeleteTag where
    deleteQuery dt =
        ( "DELETE FROM news.tag WHERE tag_id = ? RETURNING tag_id"
        , [SqlValue $ T.dtTagId dt])
    dName = T.ETag

instance DeleteSQL T.DeleteCategory where
    deleteQuery dc =
        ( "DELETE FROM news.category WHERE category_id = ? RETURNING category_id"
        , [SqlValue $ T.dcCategoryId dc])
    dName = T.ECategory

instance DeleteSQL T.DeleteAuthor where
    deleteQuery da =
        ( "DELETE FROM news.author WHERE author_id = ? RETURNING author_id"
        , [SqlValue $ T.daAuthorId da])
    dName = T.EAuthor

instance DeleteSQL T.DeleteUser where
    deleteQuery du =
        ( "DELETE FROM news.users WHERE user_id = ? RETURNING user_id"
        , [SqlValue $ T.duUserId du])
    dName = T.EUser

isAdmin :: T.User -> Bool
isAdmin u = fromMaybe False $ T.userAdmin u

instance DeleteSQL (T.WithUser T.DeleteComment) where
    deleteQuery (T.WithUser u dc) =
        let str = " DELETE FROM news.comment WHERE comment_id = ? "
            userWhere = " AND user_id = ? "
            returning = " RETURNING comment_id"
            userParam = SqlValue $ T.userId u
            commentParam = SqlValue $ T.dcCommentId dc
         in if isAdmin u
                then (str <> returning, [commentParam])
                else ( str <> userWhere <> returning
                     , [commentParam, userParam])
    dName = T.EComment

instance DeleteSQL (T.WithAuthor T.DeleteDraft) where
    deleteQuery (T.WithAuthor a (T.DeleteDraft d)) =
        ( "\
\ DELETE FROM news.draft \
\ WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
        , [SqlValue d, SqlValue a])
    dName = T.EDraft

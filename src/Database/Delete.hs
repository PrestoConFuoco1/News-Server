{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances #-}

module Database.Delete where

import Data.Maybe (fromMaybe)
import qualified Database.PostgreSQL.Simple as PS
import Database.SqlValue
import Types

class DeleteSQL a where
   deleteQuery :: a -> (PS.Query, [SqlValue])
   dName :: Entity




instance DeleteSQL DeleteTag where
   deleteQuery dt =
      ( "DELETE FROM news.tag WHERE tag_id = ? RETURNING tag_id"
      , [SqlValue $ _dt_tagId dt])
   dName = ETag




instance DeleteSQL DeleteCategory where
   deleteQuery dc =
      ( "DELETE FROM news.category WHERE category_id = ? RETURNING category_id"
      , [SqlValue $ _dc_catId dc])
   dName = ECategory




instance DeleteSQL DeleteAuthor where
   deleteQuery da =
      ( "DELETE FROM news.author WHERE author_id = ? RETURNING author_id"
      , [SqlValue $ _da_authorId da])
   dName = EAuthor




instance DeleteSQL DeleteUser where
   deleteQuery du =
      ( "DELETE FROM news.users WHERE user_id = ? RETURNING user_id"
      , [SqlValue $ _du_userId du])
   dName = EUser




isAdmin :: User -> Bool
isAdmin u = fromMaybe False $ _u_admin u

instance DeleteSQL (WithUser DeleteComment ) where
   deleteQuery (WithUser u dc) =
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
   dName = EComment




instance DeleteSQL (WithAuthor DeleteDraft) where
   deleteQuery (WithAuthor a (DeleteDraft d)) =
      ( "\
\ DELETE FROM news.draft \
\ WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
      , [SqlValue d, SqlValue a])
   dName = EDraft


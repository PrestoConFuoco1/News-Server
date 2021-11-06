{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Create
    ( CreateSQL(..)
    ) where

import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue (SqlValue(..))
import qualified Types as T

class CreateSQL a where
    createQuery :: a -> (PS.Query, [SqlValue])
    cName :: T.Entity

instance CreateSQL T.CreateTag where
    createQuery T.CreateTag {..} =
        ( "INSERT INTO news.tag (name) VALUES (?) RETURNING tag_id"
        , [SqlValue ctTagName])
    cName = T.ETag

instance CreateSQL T.CreateCategory where
    createQuery T.CreateCategory {..} =
        ( "INSERT INTO news.category (name, parent_category_Id) VALUES (?, ?) RETURNING category_id"
        , [SqlValue ccCategoryName, SqlValue ccParentCategory])
    cName = T.ECategory

instance CreateSQL T.CreateUser where
    createQuery T.CreateUser {..} =
        ( "INSERT INTO news.users (login, pass_hash, firstname, lastname) VALUES (?, ?, ?, ?) RETURNING user_id"
        , [ SqlValue cuLogin
          , SqlValue cuPassHash
          , SqlValue cuFirstName
          , SqlValue cuLastName
          ])
    cName = T.EUser

instance CreateSQL T.CreateAuthor where
    createQuery T.CreateAuthor {..} =
        ( "INSERT INTO news.author (user_id, description) VALUES (?, ?) RETURNING author_id"
        , [SqlValue caUserId, SqlValue caDescription])
    cName = T.EAuthor

instance CreateSQL (T.WithUser T.CreateComment) where
    createQuery (T.WithUser u T.CreateComment {..}) =
        ( "INSERT INTO news.comment (user_id, post_id, content) values (?, ?, ?) RETURNING comment_id"
        , [ SqlValue $ T.userId u
          , SqlValue ccomPostId
          , SqlValue ccomContent
          ])
    cName = T.EComment

instance CreateSQL T.DraftRaw where
    createQuery T.DraftRaw {..} =
        ( "INSERT INTO news.post ( title, \
                                 \ author_id,\
                                 \ category_id,\
                                 \ content,\
                                 \ photo,\
                                 \ extra_photos\
                           \ ) values (?, ?, ?, ?, ?, ?) RETURNING post_id"
        , [ SqlValue _dr_title
          , SqlValue _dr_authorId
          , SqlValue _dr_categoryId
          , SqlValue _dr_content
          , SqlValue _dr_mainPhoto
          , SqlValue $ fmap PSTy.PGArray _dr_extraPhotos
          ])
    cName = T.EPost

instance CreateSQL (T.WithAuthor T.CreateDraft) where
    createQuery (T.WithAuthor a T.CreateDraft {..}) =
        ( "INSERT INTO news.draft (title, author_id, category_id, content, photo, extra_photos) \
         \VALUES (?, ?, ?, ?, ?, ?) RETURNING draft_id"
        , [ SqlValue cdTitle
          , SqlValue a
          , SqlValue cdCategoryId
          , SqlValue cdContent
          , SqlValue cdMainPhoto
          , SqlValue $ fmap PSTy.PGArray cdExtraPhotos
          ])
    cName = T.EDraft

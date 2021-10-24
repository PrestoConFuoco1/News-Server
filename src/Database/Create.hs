{-# LANGUAGE RecordWildCards, FlexibleInstances, AllowAmbiguousTypes #-}

module Database.Create where

import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue (SqlValue(..))
import Types

class CreateSQL a where
   createQuery :: a -> (PS.Query, [SqlValue])
   cName :: Entity




instance CreateSQL CreateTag where
   createQuery CreateTag {..} =
      ( "INSERT INTO news.tag (name) VALUES (?) RETURNING tag_id"
      , [SqlValue _ct_tagName])
   cName = ETag




instance CreateSQL CreateCategory where
   createQuery CreateCategory {..} =
      ( "INSERT INTO news.category (name, parent_category_Id) VALUES (?, ?) RETURNING category_id"
      , [SqlValue _cc_catName, SqlValue _cc_parentCat])
   cName = ECategory




instance CreateSQL CreateUser where
   createQuery CreateUser {..} =
      ( "INSERT INTO news.users (login, pass_hash, firstname, lastname) VALUES (?, ?, ?, ?) RETURNING user_id"
      , [ SqlValue _cu_login
        , SqlValue _cu_passHash
        , SqlValue _cu_firstName
        , SqlValue _cu_lastName
        ])
   cName = EUser




instance CreateSQL CreateAuthor where
   createQuery CreateAuthor {..} =
      ( "INSERT INTO news.author (user_id, description) VALUES (?, ?) RETURNING author_id"
      , [SqlValue _ca_userId, SqlValue _ca_description])
   cName = EAuthor




instance CreateSQL (WithUser CreateComment) where
   createQuery (WithUser u CreateComment {..}) =
      ( "INSERT INTO news.comment (user_id, post_id, content) values (?, ?, ?) RETURNING comment_id"
      , [ SqlValue $ _u_id u
        , SqlValue _ccom_postId
        , SqlValue _ccom_content
        ])
   cName = EComment




instance CreateSQL DraftRaw where
   createQuery DraftRaw {..} =
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
   cName = EPost




instance CreateSQL (WithAuthor CreateDraft) where
   createQuery (WithAuthor a CreateDraft {..}) =
      ( "INSERT INTO news.draft (title, author_id, category_id, content, photo, extra_photos) \
         \VALUES (?, ?, ?, ?, ?, ?) RETURNING draft_id"
      , [ SqlValue _cd_title
        , SqlValue a
        , SqlValue _cd_categoryId
        , SqlValue _cd_content
        , SqlValue _cd_mainPhoto
        , SqlValue $ fmap PSTy.PGArray _cd_extraPhotos
        ])
   cName = EDraft

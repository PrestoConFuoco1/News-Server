{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Database.Read where

import qualified Data.Aeson as Ae
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue
import qualified GenericPretty as GP (PrettyShow(..))
import Prelude hiding (Read)
import Types

class ( Ae.ToJSON (MType a)
      , GP.PrettyShow (MType a)
      , PS.FromRow (MType a)
      , Show a
      , Show (MType a)
      ) =>
      Read a
   where
   type MType a :: *
   selectQuery :: a -> (PS.Query, [SqlValue])

toOffset :: Int -> Int -> Int
toOffset page size = page * size

pageingClause :: Int -> Int -> (PS.Query, [SqlValue])
pageingClause page size =
   ( " LIMIT ? OFFSET ? "
   , [SqlValue size, SqlValue $ toOffset page size])

instance Read GetPosts where
   type MType GetPosts = Post
   selectQuery (GetPosts cre tags search sortOpts) =
      let selectClause =
             "SELECT \
               \ post_id, \
               \ title, \
               \ post_creation_date, \
               \ author_id, \
               \ author_description, \
               \ user_id, \
               \ user_firstname, \
               \ user_lastname, \
               \ user_image, \ 
               \ user_login, \
               \ user_pass, \
               \ user_creation_date, \
               \ NULL as is_admin, \
               \ tagids, \
               \ tagnames, \
               \ catids, \
               \ catnames, \
               \ content, photo, extra_photos \
               \ FROM news.get_posts "
          (whereClause, args) =
             queryIntercalate " AND " $
             catMaybes
                [ fmap postsWhereDate cre
                , fmap postsWhereTags tags
                , fmap postsWhereSearch search
                ]
          orderClause = postsOrder sortOpts
       in ( selectClause <>
            notEmptyDo (" WHERE " <>) whereClause <>
            orderClause
          , args)

postsOrder :: SortOptions -> PS.Query
postsOrder (SortOptions ent order) =
   " ORDER BY " <>
   getEntity ent <> " " <> ascDescQ order <> " NULLS LAST "

getEntity :: SortEntity -> PS.Query
getEntity SEDate = " post_creation_date "
getEntity SEAuthor = " lower(author_description) "
getEntity SECategory = " lower(catnames[1]) "
getEntity SEPhotoNumber =
   " COALESCE(array_length(extra_photos, 1), 0) "

ascDescQ :: SortOrder -> PS.Query
ascDescQ SOAscending = " ASC "
ascDescQ SODescending = " DESC "

postsWhereDate ::
      CreationDateOptions -> (PS.Query, [SqlValue])
postsWhereDate (Created day) = postsWhereDate' "=" day
postsWhereDate (CreatedEarlier day) =
   postsWhereDate' "<=" day
postsWhereDate (CreatedLater day) = postsWhereDate' ">=" day

postsWhereDate' ::
      PS.Query -> Time.Day -> (PS.Query, [SqlValue])
postsWhereDate' compareSym day =
   ( " post_creation_date " <> compareSym <> " ? "
   , [SqlValue day])

postsWhereTags :: TagsOptions -> (PS.Query, [SqlValue])
postsWhereTags (OneTag tid) =
   ("? && tagids", [SqlValue $ PSTy.PGArray [tid]])
postsWhereTags (TagsIn tids) =
   ("? && tagids", [SqlValue $ PSTy.PGArray tids])
postsWhereTags (TagsAll tids) =
   ("? <@ tagids", [SqlValue $ PSTy.PGArray tids])

postsWhereSearch :: SearchOptions -> (PS.Query, [SqlValue])
postsWhereSearch (SearchOptions text) =
   let str =
          "title ILIKE ? OR content ILIKE ?\
              \ OR array_to_string(tagnames, ',') ILIKE ?\
              \ OR catnames[1] ILIKE ?"
    in ( str
       , replicate 4 $
         SqlValue $ TL.fromStrict $ enclose "%" text)

queryIntercalate ::
      PS.Query
   -> [(PS.Query, [SqlValue])]
   -> (PS.Query, [SqlValue])
queryIntercalate delim = foldr f ("", [])
  where
    f (q, v) (qacc, vacc) =
       let qu =
              notEmptyDo enclosePar q <>
              notEmptyDo (delim <>) qacc
        in (qu, v ++ vacc)

notEmptyDo :: (PS.Query -> PS.Query) -> PS.Query -> PS.Query
notEmptyDo func qu
   | qu == "" = ""
   | otherwise = func qu

enclosePar :: PS.Query -> PS.Query
enclosePar qu = "(" <> qu <> ")"

enclose :: T.Text -> T.Text -> T.Text
enclose p qu = p <> qu <> p

instance Read GetCategories where
   type MType GetCategories = Category
   selectQuery GetCategories =
      let selectClause =
             "SELECT catids, catnames FROM news.get_categories"
          args = []
       in (selectClause, args)

instance Read GetAuthors where
   type MType GetAuthors = Author
   selectQuery (GetAuthors mUser) =
      let selectClause =
             "SELECT author_id, description, user_id, firstname, lastname, \
                           \ image, login, pass, creation_date, NULL as is_admin FROM news.get_authors "
          args = []
          whereClause = " WHERE user_id = ? "
       in case mUser of
             Nothing -> (selectClause, args)
             Just user ->
                ( selectClause <> whereClause
                , [SqlValue user])

instance Read GetTags where
   type MType GetTags = Tag
   selectQuery GetTags =
      let selectClause =
             "SELECT tag_id, name \
                           \ FROM news.get_tags"
          args = []
       in (selectClause, args)

instance Read GetComments where
   type MType GetComments = Comment
   selectQuery (GetComments pid) =
      let selectClause =
             "SELECT comment_id, content,  \
                            \ user_id, firstname, lastname, image, login, \
                            \ pass_hash, creation_date, is_admin \
                            \ FROM news.get_comments WHERE post_id = ?"
          args = [SqlValue pid]
       in (selectClause, args)

instance Read (WithAuthor GetDrafts) where
   type MType (WithAuthor GetDrafts) = Draft
   selectQuery (WithAuthor a _) =
      let selectClause =
             "SELECT \
               \ draft_id, \
               \ title, \
               \ draft_creation_date, \
               \ author_id, \
               \ author_description, \
               \ user_id, \
               \ user_firstname, \
               \ user_lastname, \
               \ user_image, \ 
               \ user_login, \
               \ user_pass, \
               \ user_creation_date, \
               \ NULL as is_admin, \
               \ tagids, \
               \ tagnames, \
               \ catids, \
               \ catnames, \
               \ content, photo, extra_photos, \
               \ post_id \
               \ FROM news.get_drafts WHERE author_id = ?"
       in (selectClause, [SqlValue a])

instance Read (WithAuthor Publish) where
   type MType (WithAuthor Publish) = DraftRaw
   selectQuery (WithAuthor a (Publish draft)) =
      let selectClause =
             "SELECT \
               \ draft_id, \
               \ title, \
               \ creation_date, \
               \ author_id, \
               \ category_id, \
               \ tagids, \
               \ content, photo, extra_photos, \
               \ post_id \
               \ FROM news.draft_tag_total WHERE author_id = ? AND draft_id = ?"
       in (selectClause, [SqlValue a, SqlValue draft])

instance Read Token where
   type MType Token = User
   selectQuery y =
      let str =
             "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.get_users_by_token WHERE token = ?"
       in (str, [SqlValue y])

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Read
    ( ReadSQL(..)
    , pageingClause
    ) where

import qualified Data.Aeson as Ae
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue
import qualified GenericPretty as GP (PrettyShow(..))
import qualified Types as Y
import qualified Utils as S

class ( Ae.ToJSON (MType a)
      , GP.PrettyShow (MType a)
      , PS.FromRow (MType a)
      , Show a
      , Show (MType a)
      ) => ReadSQL a where
    type MType a :: *
    selectQuery :: a -> (PS.Query, [SqlValue])

toOffset :: Int -> Int -> Int
toOffset page size = page * size

pageingClause :: Int -> Int -> (PS.Query, [SqlValue])
pageingClause page size =
    ( " LIMIT ? OFFSET ? "
    , [SqlValue size, SqlValue $ toOffset page size])

instance ReadSQL Y.GetPosts where
    type MType Y.GetPosts = Y.Post
    selectQuery (Y.GetPosts cre tags search sortOpts) =
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
              notEmptyDo (" WHERE " <>) whereClause <> orderClause
            , args)

postsOrder :: Y.SortOptions -> PS.Query
postsOrder (Y.SortOptions ent order) =
    " ORDER BY " <>
    getEntity ent <> " " <> ascDescQ order <> " NULLS LAST "

getEntity :: Y.SortEntity -> PS.Query
getEntity Y.SEDate = " post_creation_date "
getEntity Y.SEAuthor = " lower(author_description) "
getEntity Y.SECategory = " lower(catnames[1]) "
getEntity Y.SEPhotoNumber =
    " COALESCE(array_length(extra_photos, 1), 0) "

ascDescQ :: Y.SortOrder -> PS.Query
ascDescQ Y.SOAscending = " ASC "
ascDescQ Y.SODescending = " DESC "

postsWhereDate :: Y.CreationDateOptions -> (PS.Query, [SqlValue])
postsWhereDate (Y.Created day) = postsWhereDate' "=" day
postsWhereDate (Y.CreatedEarlier day) = postsWhereDate' "<=" day
postsWhereDate (Y.CreatedLater day) = postsWhereDate' ">=" day

postsWhereDate' :: PS.Query -> Time.Day -> (PS.Query, [SqlValue])
postsWhereDate' compareSym day =
    (" post_creation_date " <> compareSym <> " ? ", [SqlValue day])

postsWhereTags :: Y.TagsOptions -> (PS.Query, [SqlValue])
postsWhereTags (Y.OneTag tid) =
    ("? && tagids", [SqlValue $ PSTy.PGArray [tid]])
postsWhereTags (Y.TagsIn tids) =
    ("? && tagids", [SqlValue $ PSTy.PGArray tids])
postsWhereTags (Y.TagsAll tids) =
    ("? <@ tagids", [SqlValue $ PSTy.PGArray tids])

postsWhereSearch :: Y.SearchOptions -> (PS.Query, [SqlValue])
postsWhereSearch (Y.SearchOptions text) =
    let str =
            "title ILIKE ? OR content ILIKE ?\
              \ OR array_to_string(tagnames, ',') ILIKE ?\
              \ OR catnames[1] ILIKE ?"
     in ( str
        , replicate 4 $ SqlValue $ TL.fromStrict $ enclose "%" text)

queryIntercalate ::
       PS.Query -> [(PS.Query, [SqlValue])] -> (PS.Query, [SqlValue])
queryIntercalate delim = foldr f ("", [])
  where
    f (q, v) (qacc, vacc) =
        let qu = notEmptyDo enclosePar q <> notEmptyDo (delim <>) qacc
         in (qu, v ++ vacc)

notEmptyDo :: (PS.Query -> PS.Query) -> PS.Query -> PS.Query
notEmptyDo func qu
    | qu == "" = ""
    | otherwise = func qu

enclosePar :: PS.Query -> PS.Query
enclosePar qu = "(" <> qu <> ")"

enclose :: T.Text -> T.Text -> T.Text
enclose p qu = p <> qu <> p

instance ReadSQL Y.GetCategories where
    type MType Y.GetCategories = Y.Category
    selectQuery (Y.GetCategories mCatId) =
        let selectClause =
                "SELECT catids, catnames FROM news.get_categories"
            args = []
            whereClause = " WHERE category_id = ? "
         in S.withMaybe mCatId (selectClause, args) $ \cid ->
                (selectClause <> whereClause, args ++ [SqlValue cid])

instance ReadSQL Y.GetAuthors where
    type MType Y.GetAuthors = Y.Author
    selectQuery (Y.GetAuthors mUser) =
        let selectClause =
                "SELECT author_id, description, user_id, firstname, lastname, \
                           \ image, login, pass, creation_date, NULL as is_admin \
                           \ FROM news.get_authors "
            args = []
            whereClause = " WHERE user_id = ? "
         in case mUser of
                Nothing -> (selectClause, args)
                Just user ->
                    ( selectClause <> whereClause
                    , args ++ [SqlValue user])

instance ReadSQL Y.GetTags where
    type MType Y.GetTags = Y.Tag
    selectQuery Y.GetTags =
        let selectClause =
                "SELECT tag_id, name \
                           \ FROM news.get_tags"
            args = []
         in (selectClause, args)

instance ReadSQL Y.GetComments where
    type MType Y.GetComments = Y.Comment
    selectQuery (Y.GetComments pid) =
        let selectClause =
                "SELECT comment_id, content,  \
                            \ user_id, firstname, lastname, image, login, \
                            \ pass_hash, creation_date, is_admin \
                            \ FROM news.get_comments WHERE post_id = ?"
            args = [SqlValue pid]
         in (selectClause, args)

instance ReadSQL (Y.WithAuthor Y.GetDrafts) where
    type MType (Y.WithAuthor Y.GetDrafts) = Y.Draft
    selectQuery (Y.WithAuthor a _) =
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

instance ReadSQL (Y.WithAuthor Y.Publish) where
    type MType (Y.WithAuthor Y.Publish) = Y.DraftRaw
    selectQuery (Y.WithAuthor a (Y.Publish draft)) =
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

instance ReadSQL Y.Token where
    type MType Y.Token = Y.User
    selectQuery y =
        let str =
                "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.get_users_by_token WHERE token = ?"
         in (str, [SqlValue y])

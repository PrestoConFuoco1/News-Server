{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Read
    ( ReadSQL(..)
    , pageingClause
    ) where

import qualified Data.Aeson as Ae
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue
import qualified GenericPretty as GP (PrettyShow(..))
import qualified Types as T
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

instance ReadSQL T.GetPosts where
    type MType T.GetPosts = T.Post
    selectQuery (T.GetPosts cre tags search sortOpts) =
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

postsOrder :: T.SortOptions -> PS.Query
postsOrder (T.SortOptions ent order) =
    " ORDER BY " <>
    getEntity ent <> " " <> ascDescQ order <> " NULLS LAST "

getEntity :: T.SortEntity -> PS.Query
getEntity T.SEDate = " post_creation_date "
getEntity T.SEAuthor = " lower(author_description) "
getEntity T.SECategory = " lower(catnames[1]) "
getEntity T.SEPhotoNumber =
    " COALESCE(array_length(extra_photos, 1), 0) "

ascDescQ :: T.SortOrder -> PS.Query
ascDescQ T.SOAscending = " ASC "
ascDescQ T.SODescending = " DESC "

postsWhereDate :: T.CreationDateOptions -> (PS.Query, [SqlValue])
postsWhereDate (T.Created day) = postsWhereDate' "=" day
postsWhereDate (T.CreatedEarlier day) = postsWhereDate' "<=" day
postsWhereDate (T.CreatedLater day) = postsWhereDate' ">=" day

postsWhereDate' :: PS.Query -> Time.Day -> (PS.Query, [SqlValue])
postsWhereDate' compareSym day =
    (" post_creation_date " <> compareSym <> " ? ", [SqlValue day])

postsWhereTags :: T.TagsOptions -> (PS.Query, [SqlValue])
postsWhereTags (T.OneTag tid) =
    ("? && tagids", [SqlValue $ PSTy.PGArray [tid]])
postsWhereTags (T.TagsIn tids) =
    ("? && tagids", [SqlValue $ PSTy.PGArray tids])
postsWhereTags (T.TagsAll tids) =
    ("? <@ tagids", [SqlValue $ PSTy.PGArray tids])

postsWhereSearch :: T.SearchOptions -> (PS.Query, [SqlValue])
postsWhereSearch (T.SearchOptions text) =
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

enclose :: Text.Text -> Text.Text -> Text.Text
enclose p qu = p <> qu <> p

instance ReadSQL T.GetCategories where
    type MType T.GetCategories = T.Category
    selectQuery (T.GetCategories mCatId) =
        let selectClause =
                "SELECT catids, catnames FROM news.get_categories"
            args = []
            whereClause = " WHERE category_id = ? "
         in S.withMaybe mCatId (selectClause, args) $ \cid ->
                (selectClause <> whereClause, args ++ [SqlValue cid])

instance ReadSQL T.GetAuthors where
    type MType T.GetAuthors = T.Author
    selectQuery (T.GetAuthors mUser) =
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

instance ReadSQL T.GetTags where
    type MType T.GetTags = T.Tag
    selectQuery T.GetTags =
        let selectClause =
                "SELECT tag_id, name \
                           \ FROM news.get_tags"
            args = []
         in (selectClause, args)

instance ReadSQL T.GetComments where
    type MType T.GetComments = T.Comment
    selectQuery (T.GetComments pid) =
        let selectClause =
                "SELECT comment_id, content,  \
                            \ user_id, firstname, lastname, image, login, \
                            \ pass_hash, creation_date, is_admin \
                            \ FROM news.get_comments WHERE post_id = ?"
            args = [SqlValue pid]
         in (selectClause, args)

instance ReadSQL (T.WithAuthor T.GetDrafts) where
    type MType (T.WithAuthor T.GetDrafts) = T.Draft
    selectQuery (T.WithAuthor a _) =
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

instance ReadSQL (T.WithAuthor T.Publish) where
    type MType (T.WithAuthor T.Publish) = T.DraftRaw
    selectQuery (T.WithAuthor a (T.Publish draft)) =
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

instance ReadSQL T.Token where
    type MType T.Token = T.User
    selectQuery y =
        let str =
                "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.get_users_by_token WHERE token = ?"
         in (str, [SqlValue y])

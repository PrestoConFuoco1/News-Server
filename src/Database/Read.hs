{-# LANGUAGE
    TypeFamilies
    , FlexibleContexts
    #-}

module Database.Read where

import Prelude hiding (Read)
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified GenericPretty as GP (PrettyShow(..))
import qualified Database.PostgreSQL.Simple as PS
import Types
import qualified Data.Aeson as Ae
import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Data.Time as Time
import Database.SqlValue
import qualified Data.Text as T

class (Ae.ToJSON (MType s), GP.PrettyShow (MType s), PS.FromRow (MType s), Show (Get s), Show (MType s))
        => Read s where
    type MType s :: * -- main type of this type family
    type Get s :: *
    selectQuery :: s -> Get s -> (PS.Query, [SqlValue])

newtype PostD = PostD ()
postDummy :: PostD
postDummy = PostD ()


toOffset :: Int -> Int -> Int
toOffset page size = page*size

pageingClause :: Int -> Int -> (PS.Query, [SqlValue])
pageingClause page size = (" LIMIT ? OFFSET ? ", [SqlValue size, SqlValue $ toOffset page size])




instance Read PostD where
    type MType PostD = Post
    type Get PostD = GetPosts
    selectQuery _ (GetPosts cre tags search sortOpts) = 
        let selectClause = "SELECT \
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
               \ FROM news.get_posts " -- ORDER BY post_creation_date DESC "
            (whereClause, args) = queryIntercalate " AND " $ catMaybes
                [fmap postsWhereDate cre,
                 fmap postsWhereTags tags,
                 fmap postsWhereSearch search]
            orderClause = postsOrder sortOpts
        in  (selectClause <> notEmptyDo (" WHERE " <>) whereClause <> orderClause, args)


postsOrder :: SortOptions -> PS.Query
postsOrder (SortOptions ent order) = " ORDER BY " <> getEntity ent <> " " <> ascDescQ order <> " NULLS LAST "

getEntity :: SortEntity -> PS.Query
getEntity SEDate = " post_creation_date "
getEntity SEAuthor = " lower(author_description) "
getEntity SECategory = " lower(catnames[1]) "
getEntity SEPhotoNumber = " COALESCE(array_length(extra_photos, 1), 0) "

ascDescQ :: SortOrder -> PS.Query
ascDescQ SOAscending = " ASC "
ascDescQ SODescending = " DESC "

{-
fromJust (Just x) = x
w :: Time.Day
w = fromJust $ Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" "2010-3-04"
-}

postsWhereDate :: CreationDateOptions -> (PS.Query, [SqlValue])
postsWhereDate (Created day)        = postsWhereDate' "="  day
postsWhereDate (CreatedEarlier day) = postsWhereDate' "<=" day
postsWhereDate (CreatedLater   day) = postsWhereDate' ">=" day

postsWhereDate' :: PS.Query -> Time.Day -> (PS.Query, [SqlValue])
postsWhereDate' compareSym day =
    (" post_creation_date " <> compareSym <> " ? ",
    [SqlValue day])


postsWhereTags :: TagsOptions -> (PS.Query, [SqlValue])
postsWhereTags (OneTag tid)   = ("? && tagids", [SqlValue $ PSTy.PGArray [tid]])
postsWhereTags (TagsIn tids)  = ("? && tagids", [SqlValue $ PSTy.PGArray tids])
postsWhereTags (TagsAll tids) = ("? <@ tagids", [SqlValue $ PSTy.PGArray tids])

postsWhereSearch :: SearchOptions -> (PS.Query, [SqlValue])
postsWhereSearch (SearchOptions text) =
    let str = "title ILIKE ? OR content ILIKE ?\
              \ OR array_to_string(tagnames, ',') ILIKE ?\
              \ OR catnames[1] ILIKE ?"
             -- \ OR array_to_string(catnames, ',') ILIKE ?"
    in  (str, replicate 4 $ SqlValue $ TL.fromStrict $ enclose "%" text)


queryIntercalate :: PS.Query -> [(PS.Query, [SqlValue])] -> (PS.Query, [SqlValue])
queryIntercalate delim = foldr f ("", [])
  where f (q, v) (qacc, vacc) = let qu = notEmptyDo enclosePar q
                                         <> notEmptyDo (delim <>) qacc
                                in (qu, v++vacc)

notEmptyDo :: (PS.Query -> PS.Query) -> PS.Query -> PS.Query
notEmptyDo func qu
    | qu == "" = ""
    | otherwise = func qu

enclosePar :: PS.Query -> PS.Query
enclosePar qu = "(" <> qu <> ")"

--enclose :: PS.Query -> PS.Query -> PS.Query
enclose :: T.Text -> T.Text -> T.Text
enclose p qu = p <> qu <> p
-----------------------------------------------------------------

newtype CatD = CatD ()
catDummy :: CatD
catDummy = CatD ()


instance Read CatD where
    type MType CatD = Category
    type Get CatD = GetCategories
    selectQuery _ (GetCategories) =
        let selectClause = "SELECT catids, catnames FROM news.get_categories"
            args = []
        in  (selectClause, args)


newtype AuthorD = AuthorD ()
authorDummy :: AuthorD
authorDummy = AuthorD ()


instance Read AuthorD where
    type MType AuthorD = Author
    type Get AuthorD = GetAuthors
    selectQuery _ (GetAuthors mUser) =
        let selectClause = "SELECT author_id, description, user_id, firstname, lastname, \
                           \ image, login, pass, creation_date, NULL as is_admin FROM news.get_authors "
            args = []
            whereClause = " WHERE user_id = ? "
        in  case mUser of
            Nothing -> (selectClause, args)
            Just user  -> (selectClause <> whereClause, [SqlValue user])

--------- other instances for getting

newtype TagD = TagD ()
tagDummy :: TagD
tagDummy = TagD ()


--class (Ae.ToJSON (MType s), GP.PrettyShow (MType s), PS.FromRow (MType s), Show (Get s), Show (MType s))
instance Read TagD where
    type MType TagD = Tag
    type Get TagD = GetTags
    selectQuery _ (GetTags) =
        let selectClause = "SELECT tag_id, name \
                           \ FROM news.get_tags"
            args = []
        in  (selectClause, args)


newtype CommentD = CommentD ()
commentDummy :: CommentD
commentDummy = CommentD ()

instance Read CommentD where
    type MType CommentD = Comment
    type Get CommentD = GetComments
    selectQuery _ (GetComments pid) =
        let selectClause = "SELECT comment_id, content,  \
                            \ user_id, firstname, lastname, image, login, \
                            \ pass_hash, creation_date, is_admin \
                            \ FROM news.get_comments WHERE post_id = ?"
            args = [SqlValue pid]
        in  (selectClause, args)



newtype DraftD = DraftD ()
draftDummy :: DraftD
draftDummy = DraftD ()


instance Read DraftD where
    type MType DraftD = Draft
    type Get DraftD = WithAuthor GetDrafts
    selectQuery _ (WithAuthor a _) = 
        let selectClause = "SELECT \
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
        in  (selectClause, [SqlValue a])

newtype DraftR = DraftR ()
draftRawDummy :: DraftR
draftRawDummy = DraftR ()


instance Read DraftR where
    type MType DraftR = DraftRaw
    type Get DraftR = WithAuthor Publish
    selectQuery _ (WithAuthor a (Publish draft)) = 
        let selectClause = "SELECT \
               \ draft_id, \
               \ title, \
               \ creation_date, \
               \ author_id, \
               \ category_id, \
               \ tagids, \
               \ content, photo, extra_photos, \
               \ post_id \
               \ FROM news.draft_tag_total WHERE author_id = ? AND draft_id = ?"
        in  (selectClause, [SqlValue a, SqlValue draft])


newtype UserTokenR = UserTokenR ()
userTokenDummy :: UserTokenR
userTokenDummy = UserTokenR ()


instance Read UserTokenR where
    type MType UserTokenR = User
    type Get UserTokenR = Token
    selectQuery _ y = 
        let str = "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.get_users_by_token WHERE token = ?"
 
        in  (str, [SqlValue y])



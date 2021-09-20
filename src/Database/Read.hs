{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts
#-}
--OverloadedStrings



module Database.Read where

import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import qualified Data.Aeson as Ae (Value, encode)

import qualified GenericPretty as GP
import GHC.Generics

import Action.RequestToAction

import qualified Logger as L
import MonadTypes
import qualified Database.PostgreSQL.Simple as PS
import qualified Types as Ty
import qualified Data.Aeson as Ae
import qualified Database.PostgreSQL.Simple.ToRow as PST
import qualified Database.PostgreSQL.Simple.ToField as PSF
import qualified Database.PostgreSQL.Simple.Types as PSTy

import qualified Data.Time as Time


import Action.Tags.Types
import Action.Category.Types
import Action.Users.Types
import Action.Authors.Types
import Action.Draft.Types
import Action.Posts.Types
import Action.Comments.Types
import ExecuteTypes
import SqlValue
import SqlQueryTypes

class (Ae.ToJSON (MType s), GP.PrettyShow (MType s), PS.FromRow (MType s), Show (Get s), Show (MType s))
        => FromSQL s where
    type MType s :: * -- main type of this type family
    type Get s :: *
    selectQuery :: s -> Get s -> (PS.Query, [SqlValue])

newtype PostD = PostD ()
postDummy = PostD ()




instance FromSQL PostD where
    type MType PostD = Ty.Post
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
               \ FROM news.get_posts "
            (whereClause, args) = queryIntercalate " AND " $ catMaybes
                [fmap postsWhereDate cre,
                 fmap postsWhereTags tags,
                 fmap postsWhereSearch search]
        in  (selectClause <> notEmptyDo (" WHERE " <>) whereClause, args)


w :: Time.Day
w = fromJust $ Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" "2010-3-04"

postsWhereClause1 :: GetPosts -> Where
postsWhereClause1 (GetPosts cre tags search sortOpts) =
--    Unit (postsWhereDate1 cre) `And` Unit (postsWhereTags1 tags)
--   `And` postsWhereSearch1 search
    foldr And WTrue $ catMaybes [fmap (Unit . postsWhereDate1) cre,
                                 fmap (Unit . postsWhereTags1) tags,
                                 fmap postsWhereSearch1 search]

postsWhereDate :: CreationDateOptions -> (PS.Query, [SqlValue])
postsWhereDate (Created day)        = postsWhereDate' "="  day
postsWhereDate (CreatedEarlier day) = postsWhereDate' "<=" day
postsWhereDate (CreatedLater   day) = postsWhereDate' ">=" day

postsWhereDate' compareSym day =
    (" post_creation_date " <> compareSym <> " ? ",
    [SqlValue day])

postsWhereDate1 :: CreationDateOptions -> WhereUnit
postsWhereDate1 (Created day)        = CompareDate Eq "post_creation_date" day
postsWhereDate1 (CreatedEarlier day) = CompareDate LEq "post_creation_date" day
postsWhereDate1 (CreatedLater   day) = CompareDate GEq "post_creation_date" day




postsWhereTags :: TagsOptions -> (PS.Query, [SqlValue])
postsWhereTags (OneTag id)   = ("? && tagids", [SqlValue $ PSTy.PGArray [id]])
postsWhereTags (TagsIn ids)  = ("? && tagids", [SqlValue $ PSTy.PGArray ids])
postsWhereTags (TagsAll ids) = ("? <@ tagids", [SqlValue $ PSTy.PGArray ids])

postsWhereTags1 :: TagsOptions -> WhereUnit
postsWhereTags1 (OneTag id) = CompareArr Overlap (ArrInt [id]) tagsarr
postsWhereTags1 (TagsIn ids) = CompareArr Overlap (ArrInt ids) tagsarr
postsWhereTags1 (TagsAll ids) = CompareArr IsContainedBy (ArrInt ids) tagsarr
tagsarr = "tagids"

postsWhereSearch :: SearchOptions -> (PS.Query, [SqlValue])
postsWhereSearch (SearchOptions text) =
    let str = "title ILIKE ? OR content ILIKE ?\
              \ OR array_to_string(tagnames, ',') ILIKE ?\
              \ OR array_to_string(catnames, ',') ILIKE ?"
    in  (str, replicate 4 $ SqlValue $ TL.fromStrict $ enclose "%" text)

postsWhereSearch1 :: SearchOptions -> Where
postsWhereSearch1 (SearchOptions text) =
    let t = enclose "%" text in
    Unit (ILike "title" t) `Or` Unit (ILike "content" t) `Or`
    Unit (ILike "array_to_string(tagnames, ',')" t) `Or`
    Unit (ILike "array_to_string(catnames, ',')" t)

queryIntercalate :: PS.Query -> [(PS.Query, [SqlValue])] -> (PS.Query, [SqlValue])
queryIntercalate delim = foldr f ("", [])
  where f (q, v) (qacc, vacc) = let qu = notEmptyDo enclosePar q
                                         <> notEmptyDo (delim <>) qacc
                                in (qu, v++vacc)

notEmptyDo func qu
    | qu == "" = ""
    | otherwise = func qu
enclosePar qu = "(" <> qu <> ")"
enclose p qu = p <> qu <> p
-----------------------------------------------------------------

newtype CatD = CatD ()
catDummy = CatD ()


instance FromSQL CatD where
    type MType CatD = Ty.Category
    type Get CatD = GetCategories
    selectQuery _ (GetCategories) =
        let selectClause = "SELECT catids, catnames FROM news.get_categories"
            args = []
        in  (selectClause, args)


newtype AuthorD = AuthorD ()
authorDummy = AuthorD ()


instance FromSQL AuthorD where
    type MType AuthorD = Ty.Author
    type Get AuthorD = GetAuthors
    selectQuery _ (GetAuthors mu) =
        let selectClause = "SELECT author_id, description, user_id, firstname, lastname, \
                           \ image, login, pass, creation_date, NULL as is_admin FROM news.get_authors "
            args = []
            whereClause = " WHERE user_id = ? "
        in  case mu of
            Nothing -> (selectClause, args)
            Just u  -> (selectClause <> whereClause, [SqlValue u])

--------- other instances for getting

newtype TagD = TagD ()
tagDummy = TagD ()


--class (Ae.ToJSON (MType s), GP.PrettyShow (MType s), PS.FromRow (MType s), Show (Get s), Show (MType s))
instance FromSQL TagD where
    type MType TagD = Ty.Tag
    type Get TagD = GetTags
    selectQuery _ (GetTags) =
        let selectClause = "SELECT tag_id, name \
                           \ FROM news.get_tags"
            args = []
        in  (selectClause, args)


newtype CommentD = CommentD ()
commentDummy = CommentD ()

instance FromSQL CommentD where
    type MType CommentD = Ty.Comment
    type Get CommentD = GetComments
    selectQuery _ (GetComments id) =
        let selectClause = "SELECT comment_id, content,  \
                            \ user_id, firstname, lastname, image, login, \
                            \ pass_hash, creation_date, is_admin \
                            \ FROM news.get_comments WHERE post_id = ?"
            args = [SqlValue id]
        in  (selectClause, args)



newtype DraftD = DraftD ()
draftDummy = DraftD ()


instance FromSQL DraftD where
    type MType DraftD = Ty.Draft
    type Get DraftD = WithAuthor GetDrafts
    selectQuery _ _ = undefined

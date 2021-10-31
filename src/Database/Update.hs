{-# LANGUAGE FlexibleContexts, RecordWildCards, FlexibleInstances,
  AllowAmbiguousTypes, TupleSections #-}

module Database.Update
    ( UpdateSQL(..)
    , intercalateWith
    , intercalateQ
    , updateParams
    ) where

import Data.Maybe (mapMaybe)
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue
import qualified Types as T

updateParams :: (UpdateSQL a) => a -> Maybe (PS.Query, [SqlValue])
updateParams ce =
    case intercalateQ $ map setUnit qs of
        Nothing -> Nothing
        Just query -> Just (query, vals)
  where
    (qs, vals) = unzip $ optionals ce

intercalateQ :: [PS.Query] -> Maybe PS.Query
intercalateQ [] = Nothing
intercalateQ xs = Just $ f xs
  where
    f [y] = y
    f (y:ys) = y <> ", " <> f ys
    f _ = error "this pattern cannot match!"

intercalateWith :: PS.Query -> [PS.Query] -> Maybe PS.Query
intercalateWith _ [] = Nothing
intercalateWith delim xs = Just $ f xs
  where
    f [y] = y
    f (y:ys) = y <> delim <> f ys
    f _ = error "this pattern cannot match!"

setUnit :: PS.Query -> PS.Query
setUnit fname = fname <> " = ?"

optionals :: (UpdateSQL a) => a -> [(PS.Query, SqlValue)]
optionals = mapMaybe f . optionalsMaybe
  where
    f (t, x) = fmap (t, ) x

class UpdateSQL a where
    updateQuery :: PS.Query -> PS.Query
    uName :: T.Entity
    optionalsMaybe :: a -> [(PS.Query, Maybe SqlValue)]
    identifParams :: a -> [SqlValue]

instance UpdateSQL T.EditTag where
    updateQuery p =
        "UPDATE news.tag SET " <>
        p <> " WHERE tag_id = ? RETURNING tag_id"
    uName = T.ETag
    optionalsMaybe T.EditTag {..} =
        [("name", Just $ SqlValue _et_tagName)]
    identifParams et = [SqlValue $ T._et_tagId et]

instance UpdateSQL T.EditCategory where
    updateQuery p =
        "UPDATE news.category SET " <>
        p <> " WHERE category_id = ? RETURNING category_id"
    uName = T.ECategory
    optionalsMaybe T.EditCategory {..} =
        [ ("name", fmap SqlValue _ec_catName)
        , ("parent_category_id", fmap SqlValue _ec_parentId)
        ]
    identifParams ec = [SqlValue $ T._ec_catId ec]

instance UpdateSQL T.EditAuthor where
    updateQuery p =
        "UPDATE news.author SET " <>
        p <> " WHERE author_id = ? RETURNING author_id"
    uName = T.EAuthor
    optionalsMaybe T.EditAuthor {..} =
        [ ("description", fmap SqlValue _ea_description)
        , ("user_id", fmap SqlValue _ea_userId)
        ]
    identifParams ea = [SqlValue $ T._ea_authorId ea]

instance UpdateSQL T.PublishEditPost where
    updateQuery p =
        "UPDATE news.post SET " <>
        p <> " WHERE post_id = ? RETURNING post_id"
    uName = T.EPost
    optionalsMaybe T.PublishEditPost {..} =
        [ ("title", Just $ SqlValue _pep_title)
        , ("category_id", Just $ SqlValue _pep_categoryId)
        , ("content", Just $ SqlValue _pep_content)
        , ("photo", fmap SqlValue _pep_mainPhoto)
        , ( "extra_photos"
          , fmap (SqlValue . PSTy.PGArray) _pep_extraPhotos)
        ]
    identifParams pep = [SqlValue $ T._pep_postId pep]

instance UpdateSQL (T.WithAuthor T.EditDraft) where
    updateQuery p =
        "UPDATE news.draft SET " <>
        p <>
        " WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
    uName = T.EDraft
    optionalsMaybe (T.WithAuthor _ T.EditDraft {..}) =
        [ ("title", fmap SqlValue _ed_title)
        , ("category_id", fmap SqlValue _ed_categoryId)
        , ("content", fmap SqlValue _ed_content)
        , ("photo", fmap SqlValue _ed_mainPhoto)
        , ( "extra_photos"
          , fmap (SqlValue . PSTy.PGArray) _ed_extraPhotos)
        ]
    identifParams (T.WithAuthor a T.EditDraft {..}) =
        [SqlValue _ed_draftId, SqlValue a]

instance UpdateSQL T.EditDraftPublish where
    updateQuery p =
        "UPDATE news.draft SET " <>
        p <> " WHERE draft_id = ? RETURNING draft_id"
    uName = T.EDraft
    optionalsMaybe T.EditDraftPublish {..} =
        [("post_id", Just $ SqlValue _edp_postId)]
    identifParams T.EditDraftPublish {..} = [SqlValue _edp_draftId]

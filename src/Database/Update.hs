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
import qualified Types as Y

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
    uName :: Y.Entity
    optionalsMaybe :: a -> [(PS.Query, Maybe SqlValue)]
    identifParams :: a -> [SqlValue]

instance UpdateSQL Y.EditTag where
    updateQuery p =
        "UPDATE news.tag SET " <>
        p <> " WHERE tag_id = ? RETURNING tag_id"
    uName = Y.ETag
    optionalsMaybe Y.EditTag {..} =
        [("name", Just $ SqlValue _et_tagName)]
    identifParams et = [SqlValue $ Y._et_tagId et]

instance UpdateSQL Y.EditCategory where
    updateQuery p =
        "UPDATE news.category SET " <>
        p <> " WHERE category_id = ? RETURNING category_id"
    uName = Y.ECategory
    optionalsMaybe Y.EditCategory {..} =
        [ ("name", fmap SqlValue _ec_catName)
        , ("parent_category_id", fmap SqlValue _ec_parentId)
        ]
    identifParams ec = [SqlValue $ Y._ec_catId ec]

instance UpdateSQL Y.EditAuthor where
    updateQuery p =
        "UPDATE news.author SET " <>
        p <> " WHERE author_id = ? RETURNING author_id"
    uName = Y.EAuthor
    optionalsMaybe Y.EditAuthor {..} =
        [ ("description", fmap SqlValue _ea_description)
        , ("user_id", fmap SqlValue _ea_userId)
        ]
    identifParams ea = [SqlValue $ Y._ea_authorId ea]

instance UpdateSQL Y.PublishEditPost where
    updateQuery p =
        "UPDATE news.post SET " <>
        p <> " WHERE post_id = ? RETURNING post_id"
    uName = Y.EPost
    optionalsMaybe Y.PublishEditPost {..} =
        [ ("title", Just $ SqlValue _pep_title)
        , ("category_id", Just $ SqlValue _pep_categoryId)
        , ("content", Just $ SqlValue _pep_content)
        , ("photo", fmap SqlValue _pep_mainPhoto)
        , ( "extra_photos"
          , fmap (SqlValue . PSTy.PGArray) _pep_extraPhotos)
        ]
    identifParams pep = [SqlValue $ Y._pep_postId pep]

instance UpdateSQL (Y.WithAuthor Y.EditDraft) where
    updateQuery p =
        "UPDATE news.draft SET " <>
        p <>
        " WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
    uName = Y.EDraft
    optionalsMaybe (Y.WithAuthor _ Y.EditDraft {..}) =
        [ ("title", fmap SqlValue _ed_title)
        , ("category_id", fmap SqlValue _ed_categoryId)
        , ("content", fmap SqlValue _ed_content)
        , ("photo", fmap SqlValue _ed_mainPhoto)
        , ( "extra_photos"
          , fmap (SqlValue . PSTy.PGArray) _ed_extraPhotos)
        ]
    identifParams (Y.WithAuthor a Y.EditDraft {..}) =
        [SqlValue _ed_draftId, SqlValue a]

instance UpdateSQL Y.EditDraftPublish where
    updateQuery p =
        "UPDATE news.draft SET " <>
        p <> " WHERE draft_id = ? RETURNING draft_id"
    uName = Y.EDraft
    optionalsMaybe Y.EditDraftPublish {..} =
        [("post_id", Just $ SqlValue _edp_postId)]
    identifParams Y.EditDraftPublish {..} = [SqlValue _edp_draftId]

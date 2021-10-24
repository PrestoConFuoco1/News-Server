{-# LANGUAGE FlexibleContexts, RecordWildCards, FlexibleInstances, AllowAmbiguousTypes #-}

module Database.Update where

import Data.Maybe (mapMaybe)
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue
import Types

updateParams ::
      (UpdateSQL a)
   => a
   -> Maybe (PS.Query, [SqlValue])
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

intercalateWith :: PS.Query -> [PS.Query] -> Maybe PS.Query
intercalateWith _ [] = Nothing
intercalateWith delim xs = Just $ f xs
  where
    f [y] = y
    f (y:ys) = y <> delim <> f ys

setUnit :: PS.Query -> PS.Query
setUnit fname = fname <> " = ?"

optionals ::
      (UpdateSQL a) => a -> [(PS.Query, SqlValue)]
optionals = mapMaybe f . optionalsMaybe
  where
    f (t, x) = fmap (\a -> (t, a)) x

class UpdateSQL a where
   updateQuery :: PS.Query -> PS.Query
   uName :: Entity
   optionalsMaybe ::
         a -> [(PS.Query, Maybe SqlValue)]
   identifParams :: a -> [SqlValue]




instance UpdateSQL EditTag where
   updateQuery p =
      "UPDATE news.tag SET " <>
      p <> " WHERE tag_id = ? RETURNING tag_id"
   uName = ETag
   optionalsMaybe EditTag {..} =
      [("name", Just $ SqlValue _et_tagName)]
   identifParams et = [SqlValue $ _et_tagId et]




instance UpdateSQL EditCategory where
   updateQuery p =
      "UPDATE news.category SET " <>
      p <> " WHERE category_id = ? RETURNING category_id"
   uName = ECategory
   optionalsMaybe EditCategory {..} =
      [ ("name", fmap SqlValue _ec_catName)
      , ("parent_category_id", fmap SqlValue _ec_parentId)
      ]
   identifParams ec = [SqlValue $ _ec_catId ec]




instance UpdateSQL EditAuthor where
   updateQuery p =
      "UPDATE news.author SET " <>
      p <> " WHERE author_id = ? RETURNING author_id"
   uName = EAuthor
   optionalsMaybe EditAuthor {..} =
      [ ("description", fmap SqlValue _ea_description)
      , ("user_id", fmap SqlValue _ea_userId)
      ]
   identifParams ea = [SqlValue $ _ea_authorId ea]




instance UpdateSQL PublishEditPost where
   updateQuery p =
      "UPDATE news.post SET " <>
      p <> " WHERE post_id = ? RETURNING post_id"
   uName = EPost
   optionalsMaybe PublishEditPost {..} =
      [ ("title", Just $ SqlValue _pep_title)
      , ("category_id", Just $ SqlValue _pep_categoryId)
      , ("content", Just $ SqlValue _pep_content)
      , ("photo", fmap SqlValue _pep_mainPhoto)
      , ( "extra_photos"
        , fmap (SqlValue . PSTy.PGArray) _pep_extraPhotos)
      ]
   identifParams pep = [SqlValue $ _pep_postId pep]




instance UpdateSQL (WithAuthor EditDraft) where
   updateQuery p =
      "UPDATE news.draft SET " <>
      p <>
      " WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
   uName = EDraft
   optionalsMaybe (WithAuthor _ EditDraft {..}) =
      [ ("title", fmap SqlValue _ed_title)
      , ("category_id", fmap SqlValue _ed_categoryId)
      , ("content", fmap SqlValue _ed_content)
      , ("photo", fmap SqlValue _ed_mainPhoto)
      , ( "extra_photos"
        , fmap (SqlValue . PSTy.PGArray) _ed_extraPhotos)
      ]
   identifParams (WithAuthor a EditDraft {..}) =
      [SqlValue _ed_draftId, SqlValue a]




instance UpdateSQL EditDraftPublish where
   updateQuery p =
      "UPDATE news.draft SET " <>
      p <> " WHERE draft_id = ? RETURNING draft_id"
   uName = EDraft
   optionalsMaybe EditDraftPublish {..} =
      [("post_id", Just $ SqlValue _edp_postId)]
   identifParams EditDraftPublish {..} =
      [SqlValue _edp_draftId]

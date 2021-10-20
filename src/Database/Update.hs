{-# LANGUAGE TypeFamilies, FlexibleContexts, RecordWildCards #-}

module Database.Update where

import Data.Maybe (mapMaybe)
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue
import Types

updateParams ::
      (UpdateSQL s)
   => s
   -> Upd s
   -> Maybe (PS.Query, [SqlValue])
updateParams s ce =
   case intercalateQ $ map setUnit qs of
      Nothing -> Nothing
      Just query -> Just (query, vals)
  where
    (qs, vals) = unzip $ optionals s ce

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
      (UpdateSQL s) => s -> Upd s -> [(PS.Query, SqlValue)]
--optionals s = catMaybes . map f . optionalsMaybe s
optionals s = mapMaybe f . optionalsMaybe s
  where
    f (t, x) = fmap (\a -> (t, a)) x

class UpdateSQL s where
   type Upd s :: *
   updateQuery :: s -> PS.Query -> PS.Query
   uName :: s -> Entity
   optionalsMaybe ::
         s -> Upd s -> [(PS.Query, Maybe SqlValue)]
   identifParams :: s -> Upd s -> [SqlValue]

newtype UTag =
   UTag ()

dummyUTag :: UTag
dummyUTag = UTag ()

instance UpdateSQL UTag where
   type Upd UTag = EditTag
   updateQuery _ p =
      "UPDATE news.tag SET " <>
      p <> " WHERE tag_id = ? RETURNING tag_id"
   uName _ = ETag
   optionalsMaybe _ EditTag {..} =
      [("name", Just $ SqlValue _et_tagName)]
   identifParams _ et = [SqlValue $ _et_tagId et]

newtype UCat =
   UCat ()

dummyUCat :: UCat
dummyUCat = UCat ()

instance UpdateSQL UCat where
   type Upd UCat = EditCategory
   updateQuery _ p =
      "UPDATE news.category SET " <>
      p <> " WHERE category_id = ? RETURNING category_id"
   uName _ = ECategory
   optionalsMaybe _ EditCategory {..} =
      [ ("name", fmap SqlValue _ec_catName)
      , ("parent_category_id", fmap SqlValue _ec_parentId)
      ]
   identifParams _ ec = [SqlValue $ _ec_catId ec]

newtype UAuthor =
   UAuthor ()

dummyUAuthor :: UAuthor
dummyUAuthor = UAuthor ()

instance UpdateSQL UAuthor where
   type Upd UAuthor = EditAuthor
   updateQuery _ p =
      "UPDATE news.author SET " <>
      p <> " WHERE author_id = ? RETURNING author_id"
   uName _ = EAuthor
   optionalsMaybe _ EditAuthor {..} =
      [ ("description", fmap SqlValue _ea_description)
      , ("user_id", fmap SqlValue _ea_userId)
      ]
   identifParams _ ea = [SqlValue $ _ea_authorId ea]

newtype UPost =
   UPost ()

dummyUPost :: UPost
dummyUPost = UPost ()

instance UpdateSQL UPost where
   type Upd UPost = PublishEditPost
   updateQuery _ p =
      "UPDATE news.post SET " <>
      p <> " WHERE post_id = ? RETURNING post_id"
   uName _ = EPost
   optionalsMaybe _ PublishEditPost {..} =
      [ ("title", Just $ SqlValue _pep_title)
      , ("category_id", Just $ SqlValue _pep_categoryId)
      , ("content", Just $ SqlValue _pep_content)
      , ("photo", fmap SqlValue _pep_mainPhoto)
      , ( "extra_photos"
        , fmap (SqlValue . PSTy.PGArray) _pep_extraPhotos)
      ]
   identifParams _ pep = [SqlValue $ _pep_postId pep]

newtype UDraft =
   UDraft ()

draftEditDummy :: UDraft
draftEditDummy = UDraft ()

instance UpdateSQL UDraft where
   type Upd UDraft = WithAuthor EditDraft
   updateQuery _ p =
      "UPDATE news.draft SET " <>
      p <>
      " WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
   uName _ = EDraft
   optionalsMaybe _ (WithAuthor _ EditDraft {..}) =
      [ ("title", fmap SqlValue _ed_title)
      , ("category_id", fmap SqlValue _ed_categoryId)
      , ("content", fmap SqlValue _ed_content)
      , ("photo", fmap SqlValue _ed_mainPhoto)
      , ( "extra_photos"
        , fmap (SqlValue . PSTy.PGArray) _ed_extraPhotos)
      ]
   identifParams _ (WithAuthor a EditDraft {..}) =
      [SqlValue _ed_draftId, SqlValue a]

newtype UPDraft =
   UPDraft ()

draftEditPublishDummy :: UPDraft
draftEditPublishDummy = UPDraft ()

instance UpdateSQL UPDraft where
   type Upd UPDraft = EditDraftPublish
   updateQuery _ p =
      "UPDATE news.draft SET " <>
      p <> " WHERE draft_id = ? RETURNING draft_id"
   uName _ = EDraft
   optionalsMaybe _ EditDraftPublish {..} =
      [("post_id", Just $ SqlValue _edp_postId)]
   identifParams _ EditDraftPublish {..} =
      [SqlValue _edp_draftId]

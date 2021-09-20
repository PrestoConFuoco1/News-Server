{-# LANGUAGE
TypeFamilies,
FlexibleContexts,
RecordWildCards
#-}


module Database.Update where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Aeson as Ae (Value, encode)

import qualified Database.PostgreSQL.Simple as PS

import Action.Tags.Types
import Action.Authors.Types
import Action.Category.Types
import Database.SqlValue
import Data.Maybe (catMaybes)

updateParams :: (UpdateSQL s) => s -> Upd s -> Maybe (PS.Query, [SqlValue])
updateParams s ce = case intercalateQ $ map setUnit qs of
        Nothing -> Nothing
        Just query -> Just (query, vals)
  where  (qs, vals) = unzip $ optionals s ce

intercalateQ :: [PS.Query] -> Maybe PS.Query
intercalateQ [] = Nothing
intercalateQ xs = Just $ f xs
  where
        f [x] = x
        f (x:xs) = x <> ", " <> f xs
 

setUnit fname = fname <> " = ?"

optionals :: (UpdateSQL s) => s -> Upd s -> [(PS.Query, SqlValue)]
optionals s = catMaybes . map f . optionalsMaybe s
  where
        f (t, x) = fmap (\a -> (t, a)) x
{-
optionalsMaybe :: EditCategory -> [(PS.Query, Maybe SqlValue)]
optionalsMaybe EditCategory{..} =
            [("name", fmap SqlValue _ec_catName),
             ("parent_category_id", fmap SqlValue _ec_parentId)]
 -}

class UpdateSQL s where
    type Upd s :: *
    updateQuery :: s -> PS.Query -> PS.Query
    uName :: s -> B.ByteString
    optionalsMaybe :: s -> Upd s -> [(PS.Query, Maybe SqlValue)]
    identifParams :: s -> Upd s -> [SqlValue]


newtype UTag = UTag ()
dummyUTag = UTag ()

instance UpdateSQL UTag where
    type Upd UTag = EditTag
    updateQuery _ = \p -> "UPDATE news.tag SET " <> p <> " WHERE tag_id = ?"
    uName _ = "tag"
    optionalsMaybe _ EditTag{..} =
            [("name", Just $ SqlValue _et_tagName)]
    identifParams _ et = [SqlValue $ _et_tagId et]

   


newtype UCat = UCat ()
dummyUCat = UCat ()

instance UpdateSQL UCat where
    type Upd UCat = EditCategory
    updateQuery _ = \p -> "UPDATE news.category SET " <> p <> " WHERE category_id = ?"
    uName _ = "category"
    optionalsMaybe _ EditCategory{..} =
            [("name", fmap SqlValue _ec_catName),
             ("parent_category_id", fmap SqlValue _ec_parentId)]
    identifParams _ ec = [SqlValue $ _ec_catId ec]



newtype UAuthor = UAuthor ()
dummyUAuthor = UAuthor ()

instance UpdateSQL UAuthor where
    type Upd UAuthor = EditAuthor
    updateQuery _ = \p -> "UPDATE news.author SET " <> p <> " WHERE author_id = ?"
    uName _ = "author"
    optionalsMaybe _ EditAuthor{..} =
            [("description", fmap SqlValue _ea_description),
             ("user_id", fmap SqlValue _ea_userId)]
    identifParams _ ea = [SqlValue $ _ea_authorId ea]














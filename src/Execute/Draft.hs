{-# LANGUAGE RecordWildCards,
             TypeFamilies #-}
module Execute.Draft where


import qualified Data.Text as T (pack, Text)

import qualified Database.PostgreSQL.Simple.Types as PSTy
--import qualified GenericPretty as GP

import Database.Update

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import Execute.Types
import Execute.Utils
import Action.Draft.Types


import Database.SqlValue
import Execute.Actions

import Database.Update
import ActWithOne

import qualified Exceptions as Ex

{-
class UpdateSQL s where
    type Upd s :: *
    updateQuery :: s -> PS.Query -> PS.Query
    uName :: s -> B.ByteString
    optionalsMaybe :: s -> Upd s -> [(PS.Query, Maybe SqlValue)]
    identifParams :: s -> Upd s -> [SqlValue]
-}


newtype UDraft = UDraft ()
draftEditDummy = UDraft ()

instance UpdateSQL UDraft where
    type Upd UDraft = WithAuthor EditDraft
    updateQuery _ =
        \p -> "UPDATE news.draft SET " <> p <> " WHERE draft_id = ? AND author_id = ?"
    uName _ = "draft"
    optionalsMaybe _ (WithAuthor _ EditDraft{..}) =
        [("title", fmap SqlValue _ed_title),
         ("tags", fmap (SqlValue . PSTy.PGArray) _ed_tags),
         ("category_id", fmap SqlValue _ed_categoryId),
         ("content", fmap SqlValue _ed_content),
         ("main_photo", fmap SqlValue _ed_mainPhoto),
         ("extra_photos", fmap (SqlValue . PSTy.PGArray) _ed_extraPhotos)]

    identifParams _ (WithAuthor a EditDraft{..}) =
        [SqlValue _ed_draftId, SqlValue a]

createDraft :: (MonadServer m) => WithAuthor CreateDraft -> m Response -- ?
createDraft (WithAuthor a CreateDraft{..}) = do
    let str =
         "INSERT INTO news.draft (title, author_id, category_id, content, photo, extra_photos) \
         \VALUES (?, ?, ?, ?, ?, ?) RETURNING draft_id"
        args = [SqlValue _cd_title, SqlValue a,
                SqlValue _cd_categoryId, SqlValue _cd_content,
                SqlValue _cd_mainPhoto, SqlValue $ fmap PSTy.PGArray _cd_extraPhotos]
    
    debugStr <- formatQuery str args
    logDebug $ T.pack $ show debugStr

--    id <- query str args >>= fmap (map PSTy.fromOnly) >>= \is -> validateUnique (Ex.invalidUnique is) $ is
    ids <- fmap (map PSTy.fromOnly) $ query str args
    id <- validateUnique (logError "Failed to create draft" >> Ex.throwDefault) ids
    logDebug $ "Created draft with id = " <> (T.pack $ show id)
    tags <- attachTagsToDraft id _cd_tags
    return (ok $ "Draft successfully created")

editDraft :: (MonadServer m) => WithAuthor EditDraft -> m Response -- ?
editDraft x@(WithAuthor a EditDraft{..}) = do
    let s = draftEditDummy
    num <- editThis' s x
    actWithOne (AWOu s) num
    case _ed_tags of
        Nothing -> return ()
        Just tags -> do
            ts <- attachTagsToDraft _ed_draftId tags
            return ()
    return (ok $ "Draft successfully edited")
   
{-
-}
    
    

attachTagsToDraft :: (MonadServer m) => Int -> [Int] -> m [Int]
attachTagsToDraft draftId tagsIds = do
    let str =
         "INSERT INTO news.draft_tag (draft_id, tag_id) VALUES "
        returning = " RETURNING tag_id"
        count = length tagsIds
        insertUnit = " ( ?, ? ) "
        insertUnits = maybe "" id $ intercalateQ $ replicate count insertUnit
        insertParams = map SqlValue $ foldr f [] tagsIds
        f x acc = draftId : x : acc
        qu = str <> insertUnits <> returning
    debugStr <- formatQuery qu insertParams
    logDebug $ T.pack $ show debugStr

    ids <- fmap (map PSTy.fromOnly) $ query qu insertParams
    logDebug $ "Attached tags with id in " <> (T.pack $ show ids)
    return ids



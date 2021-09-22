{-# LANGUAGE RecordWildCards,
             TypeFamilies #-}
module Execute.Draft where


import qualified Data.Text as T (pack, Text)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Database.PostgreSQL.Simple as PS
--import qualified GenericPretty as GP

import Database.Update

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import Execute.Types
import Execute.Utils
import Action.Draft


import Database.SqlValue
import Execute.Actions

import Database.Update
import ActWithOne

import qualified Exceptions as Ex

import qualified Data.Aeson as Ae (Value(..))

newtype UDraft = UDraft ()
draftEditDummy = UDraft ()

instance UpdateSQL UDraft where
    type Upd UDraft = WithAuthor EditDraft
    updateQuery _ =
        \p -> "UPDATE news.draft SET " <> p <> " WHERE draft_id = ? AND author_id = ?"
    uName _ = "draft"
    optionalsMaybe _ (WithAuthor _ EditDraft{..}) =
        [("title", fmap SqlValue _ed_title),
    --     ("tags", fmap (SqlValue . PSTy.PGArray) _ed_tags),
         ("category_id", fmap SqlValue _ed_categoryId),
         ("content", fmap SqlValue _ed_content),
         ("photo", fmap SqlValue _ed_mainPhoto),
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
    --return (ok "Draft successfully created" )
    return $ okCreated ("Draft successfully created. " <> idInResult) id

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
    return (ok "Draft successfully edited" Ae.Null)
   
{-
-}

publish :: (MonadServer m) => WithAuthor Publish -> m Response
publish (WithAuthor a Publish{..}) = do
    let str = " INSERT INTO news.post (title, author_id, category_id, content, photo, extra_photos) \
              \ SELECT d.title, d.author_id, d.category_id, d.content, d.photo, d.extra_photos FROM news.draft d \
              \ WHERE d.author_id = ? AND d.draft_id = ? \
              \ RETURNING post_id "
        params = [SqlValue a, SqlValue _p_draftId]
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    ids <- fmap (map PSTy.fromOnly) $ query str params
    id <- validateUnique (logError "Failed to create post" >> Ex.throwDefault) ids
    logDebug $ "Created post with id = " <> (T.pack $ show id)
    tags <- attachTagsToPost _p_draftId id
    return (ok "Post successfully created" Ae.Null)


attachTagsToPost :: (MonadServer m) => Int -> Int -> m [Int]
attachTagsToPost draftId postId = do
    let str =
          "INSERT INTO news.post_tag (post_id, tag_id) \
         \ SELECT ?, dt.tag_id FROM news.draft_tag dt WHERE dt.draft_id = ? \
         \ ON CONFLICT ON CONSTRAINT post_tag_post_id_tag_id_key DO NOTHING \
         \ RETURNING tag_id "
        params = [SqlValue postId, SqlValue draftId]
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    ids <- fmap (map PSTy.fromOnly) $ query str params
    logDebug $ "Attached tags with id in " <> (T.pack $ show ids) <> " to post with id = " <> (T.pack $ show postId)
    return ids


    
    

attachTagsToDraft :: (MonadServer m) => Int -> [Int] -> m [Int]
attachTagsToDraft draftId [] = removeTagsFromDraft draftId >> return []
attachTagsToDraft draftId tagsIds = do
    let str =
         "INSERT INTO news.draft_tag (draft_id, tag_id) VALUES "
        returning = " ON CONFLICT ON CONSTRAINT draft_tag_draft_id_tag_id_key \
                    \  DO NOTHING RETURNING tag_id"
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

    let str =
         "DELETE FROM news.draft_tag WHERE draft_id = ? AND NOT tag_id IN ?"
        params = [SqlValue draftId, SqlValue $ PS.In tagsIds]
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    num <- execute str params
    logDebug $ "Removed " <> (T.pack $ show num) <> " tags from draft with id = " <> (T.pack $ show draftId)
    return ids

removeTagsFromDraft :: (MonadServer m) => Int -> m Int
removeTagsFromDraft draftId = do
    let str =
            "DELETE FROM news.draft_tag WHERE draft_id = ?"
        params = [SqlValue draftId]
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    num <- execute str params
    logDebug $ "Removed " <> (T.pack $ show num) <> " tags from draft with id = " <> (T.pack $ show draftId)
    return num


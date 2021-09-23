{-# LANGUAGE TypeFamilies,
             RecordWildCards #-}
module Execute.Draft where


import qualified Data.Text as T (pack, Text)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Database.PostgreSQL.Simple as PS
--import qualified GenericPretty as GP

import Database.Update

import MonadTypes (MonadServer (..), MonadSQL(..),logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import Execute.Types
import Execute.Utils
import Action.Draft


import Database.SqlValue
import Execute.Actions
import Database.Create
import Database.Update
--import ActWithOne
import Execute.Result
import Execute.HasTags
import qualified Exceptions as Ex

import qualified Data.Aeson as Ae (Value(..))
import Database.Read
import Action.Posts

import qualified Types as Ty

newtype CDraft = CDraft ()
draftCreateDummy = CDraft ()


instance CreateSQL CDraft where
    type Create CDraft = WithAuthor CreateDraft
    createQuery s (WithAuthor a CreateDraft{..}) = ("INSERT INTO news.draft (title, author_id, category_id, content, photo, extra_photos) \
         \VALUES (?, ?, ?, ?, ?, ?) RETURNING draft_id",
        [SqlValue _cd_title, SqlValue a, SqlValue _cd_categoryId, SqlValue _cd_content,
         SqlValue _cd_mainPhoto, SqlValue $ fmap PSTy.PGArray _cd_extraPhotos])
    cName _ = "draft"
    cUniqueField _ = "for what the fuck i created this function? it's wothless"
    cForeign _ = "author_id or category_id"


createDraft :: (MonadServer m) => WithAuthor CreateDraft -> m Response
createDraft x@(WithAuthor a CreateDraft{..}) = Ex.withHandler Ex.draftCreateHandler $
                                                withTransaction $ do
    draft <- createThis' draftCreateDummy x
  --  logDebug $ "Created draft with id = " <> (T.pack $ show draft)
    tags <- attachTags dummyHDraft draft _cd_tags
    logInfo $ attached "draft" tags draft
    return $ okCreated ("Draft successfully created. " <> idInResult) draft


editDraft :: (MonadServer m) => WithAuthor EditDraft -> m Response
editDraft x@(WithAuthor a EditDraft{..}) = Ex.withHandler Ex.draftEditHandler $
                                                withTransaction $ do
    let s = draftEditDummy
    draft <- editThis' s x
    --actWithOne (AWOu s) num
    case _ed_tags of
        Nothing -> return ()
        Just tags -> do
            ts <- attachTags dummyHDraft draft tags
            logInfo $ attached "draft" ts draft
            tsRem <- removeAllButGivenTags dummyHDraft draft tags
            logInfo $ removed "draft" tsRem draft
 
    return (ok "Draft successfully edited" Ae.Null)

newtype UDraft = UDraft ()
draftEditDummy = UDraft ()

instance UpdateSQL UDraft where
    type Upd UDraft = WithAuthor EditDraft
    updateQuery _ =
        \p -> "UPDATE news.draft SET " <> p <> " WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
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


newtype UPDraft = UPDraft ()
draftEditPublishDummy = UPDraft ()

instance UpdateSQL UPDraft where
    type Upd UPDraft = EditDraftPublish
    updateQuery _ =
        \p -> "UPDATE news.draft SET " <> p <> " WHERE draft_id = ? RETURNING draft_id"
    uName _ = "draft"
    optionalsMaybe _ EditDraftPublish{..} =
        [("post_id", Just $ SqlValue _edp_postId)]

    identifParams _ EditDraftPublish{..} =
        [SqlValue _edp_draftId]





{-
publish1 :: (MonadServer m) => WithAuthor Publish -> m Response
publish1 (WithAuthor a Publish{..}) = do
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
    tags <- attachTags _p_draftId id
    return (ok "Post successfully created" Ae.Null)
-}
publish :: (MonadServer m) => WithAuthor Publish -> m Response
publish x@(WithAuthor a Publish{..}) = Ex.withHandler Ex.publishHandler $
                                       withTransaction $ do
    drafts <- getThis' draftRawDummy x
    draft <- validateUnique (Ex.throwDraftNotFound _p_draftId) drafts
    case Ty._dr_postId draft of
        Nothing -> publishCreate draft
        Just post -> publishEdit post draft
{-
func :: Ty.DraftRaw -> EditDraftPublish
func Ty.DraftRaw{..} = EditDraftPublish { _edp_postId = _dr_postId, _edp_draftId = _dr_draftId }
-}
publishCreate :: (MonadServer m) => Ty.DraftRaw -> m Response
publishCreate x = do
    post <- createThis' dummyCPost x
    logInfo $ "Created post with id = " <> showText post
    draft <- editThis' draftEditPublishDummy (EditDraftPublish post $ Ty._dr_draftId x)
    logInfo $ "Added post_id to draft with id = " <> showText draft
    tags_ <- attachTags dummyHPost post (Ty._dr_tagIds x)
    logInfo $ attached "post" tags_ post
    return $ okCreated "Post successfully created" post

func :: Int -> Ty.DraftRaw -> PublishEditPost
func post Ty.DraftRaw{..} =
    let _pep_postId = post
        _pep_title = _dr_title
  --      _pep_creationDate = _dr_creationDate
 --       _pep_authorId = _dr_authorId
        _pep_categoryId = _dr_categoryId
        _pep_content = _dr_content
        _pep_mainPhoto = _dr_mainPhoto
        _pep_extraPhotos = _dr_extraPhotos
    in PublishEditPost{..}
 

publishEdit :: (MonadServer m) => Int -> Ty.DraftRaw -> m Response
publishEdit post draft = do
    let s = dummyUPost
        publishPost = func post draft
    post_ <- editThis' s publishPost
    ts <- attachTags dummyHPost post $ Ty._dr_tagIds draft
    logInfo $ attached "post" ts post
    tsRem <- removeAllButGivenTags dummyHPost post $ Ty._dr_tagIds draft
    logInfo $ removed "post" tsRem draft
    return $ okCreated "Draft successfully published, post_id is in \"result\"" post

 {-
editDraft :: (MonadServer m) => WithAuthor EditDraft -> m Response
editDraft x@(WithAuthor a EditDraft{..}) = do
    let s = draftEditDummy
    drafts <- editThis' s x
    draft <- validateUnique (Ex.throwBadInsert "draft") drafts
    --actWithOne (AWOu s) num
    case _ed_tags of
        Nothing -> return ()
        Just tags -> do
            ts <- attachTags dummyHDraft draft tags
            logInfo $ attached "draft" ts draft
            tsRem <- removeAllButGivenTags dummyHDraft draft tags
            logInfo $ removed "draft" tsRem draft
 
    return (ok "Draft successfully edited" Ae.Null)
-}
   
    





 
showText :: (Show a) => a -> T.Text
showText = T.pack . show

attached ent ids id = "Attached tags with id in " <> showText ids <> " to " <> ent <> " with id = " <> showText id
removed ent ids id = "Removed tags with id in " <> showText ids <> " to " <> ent <> " with id = " <> showText id



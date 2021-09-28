{-# LANGUAGE TypeFamilies,
             RecordWildCards #-}
module Execute.Draft where


import qualified Data.Text as T (pack, Text)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Database.PostgreSQL.Simple as PS


import MonadTypes (
        execute, query, formatQuery)
import Execute.Types
import MonadLog
--import Execute.Utils

import MonadNews
import MonadNewsInstances
import Database.SqlValue
--import Execute.Actions
import Database.Create
import Database.Update
import Result
--import Execute.HasTags
import qualified Exceptions as Ex

import qualified Data.Aeson as Ae (Value(..))
import Database.Read

import Types
import Execute.Database
import Execute.Utils

createDraft :: (MonadNews m) => WithAuthor CreateDraft -> m Response
createDraft x@(WithAuthor a CreateDraft{..}) = Ex.withHandler Ex.draftCreateHandler $
                                                withTransaction' $ do
    draft <- createDraftN x
  --  logDebug $ "Created draft with id = " <> (T.pack $ show draft)
    tags <- attachTagsToDraft draft _cd_tags
    logInfo $ attached "draft" tags draft
    return $ okCreated ("Draft successfully created. " <> idInResult) draft

editDraft :: (MonadNews m) => WithAuthor EditDraft -> m Response
editDraft x@(WithAuthor a EditDraft{..}) = Ex.withHandler Ex.draftEditHandler $
                                                withTransaction' $ do
    draft <- editDraftN x
    case _ed_tags of
        Nothing -> return ()
        Just tags -> do
            ts <- attachTagsToDraft draft tags
            logInfo $ attached "draft" ts draft
            tsRem <- removeAllButGivenTagsDraft draft tags
            logInfo $ removed "draft" tsRem draft
 
    return (ok "Draft successfully edited" Ae.Null)

publish :: (MonadNews m) => WithAuthor Publish -> m Response
publish x@(WithAuthor a Publish{..}) = Ex.withHandler Ex.publishHandler $
                                       withTransaction' $ do
    drafts <- getDraftsRaw x
    draft <- validateUnique (Ex.throwDraftNotFound _p_draftId) drafts
    case _dr_postId draft of
        Nothing -> publishCreate draft
        Just post -> publishEdit post draft


publishCreate :: (MonadNews m) => DraftRaw -> m Response
publishCreate x = do
    post <- createPost x
    logInfo $ "Created post with id = " <> showText post
    draft <- editDraftPublish (EditDraftPublish post $ _dr_draftId x)
    logInfo $ "Added post_id to draft with id = " <> showText draft
    tags_ <- attachTagsToPost post (_dr_tagIds x)
    logInfo $ attached "post" tags_ post
    return $ okCreated "Post successfully created" post

publishEdit :: (MonadNews m) => Int -> DraftRaw -> m Response
publishEdit post draft = do
    let
        publishPost = func post draft
    post_ <- editPostPublish publishPost
    ts <- attachTagsToPost post $ _dr_tagIds draft
    logInfo $ attached "post" ts post
    tsRem <- removeAllButGivenTagsPost post $ _dr_tagIds draft
    logInfo $ removed "post" tsRem draft
    return $ okCreated "Draft successfully published, post_id is in \"result\"" post



func :: Int -> DraftRaw -> PublishEditPost
func post DraftRaw{..} =
    let _pep_postId = post
        _pep_title = _dr_title
        _pep_categoryId = _dr_categoryId
        _pep_content = _dr_content
        _pep_mainPhoto = _dr_mainPhoto
        _pep_extraPhotos = _dr_extraPhotos
    in PublishEditPost{..}




 
showText :: (Show a) => a -> T.Text
showText = T.pack . show

attached ent ids id = "Attached tags with id in " <> showText ids <> " to " <> ent <> " with id = " <> showText id
removed ent ids id = "Removed tags with id in " <> showText ids <> " to " <> ent <> " with id = " <> showText id



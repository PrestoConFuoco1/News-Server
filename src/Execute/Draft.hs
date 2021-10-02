{-# LANGUAGE TypeFamilies,
             RecordWildCards #-}
module Execute.Draft where


import qualified Data.Text as T (pack, Text)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Database.PostgreSQL.Simple as PS


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
import qualified Control.Monad.Catch as CMC
{-
draftActionHandler :: (MonadThrow m, MonadLog m) => T.Text -> SomeException -> m a
draftActionHandler action e =
    let str = "Failed to " <> action <> " draft."
    in  logError (str <> ", all changes are discarded")
                            >> CMC.throwM e

publishHandler :: (MonadThrow m, MonadLog m) => SomeException -> m a
publishHandler e = draftActionHandler "publish" e
draftCreateHandler :: (MonadThrow m, MonadLog m) => SomeException -> m a
draftCreateHandler e = draftActionHandler "create" e
draftEditHandler :: (MonadThrow m, MonadLog m) => SomeException -> m a
draftEditHandler e = draftActionHandler "edit" e
-}


draftModifyErrorToApiResult :: DraftModifyError -> APIResult
draftModifyErrorToApiResult (DModifyError x) = modifyErrorToApiResult EDraft x
draftModifyErrorToApiResult (DTagsError (TagsAttachError (ForeignViolation field value))) = RInvalidTag value

draftModifyHandler :: (MonadNews m) => DraftModifyError -> m APIResult
draftModifyHandler = return . draftModifyErrorToApiResult

createDraft :: (MonadNews m) => WithAuthor CreateDraft -> m APIResult
createDraft x@(WithAuthor a CreateDraft{..}) = --Ex.withHandler draftCreateHandler $
  Ex.withHandler draftModifyHandler $ withTransaction' $ do
    eithDraft <- createDraftN x
  --  logDebug $ "Created draft with id = " <> (T.pack $ show draft)
    case eithDraft of
        Left err -> CMC.throwM $ DModifyError err
        Right draft -> do
            eithTags <- attachTagsToDraft draft _cd_tags
            case eithTags of
              Left err -> CMC.throwM $ DTagsError err 
            --logInfo $ attached "draft" tags draft
              Right tags -> return $ RCreated EDraft draft
--return $ okCreated ("Draft successfully created. " <> idInResult) draft

editDraft :: (MonadNews m) => WithAuthor EditDraft -> m APIResult
editDraft x@(WithAuthor a EditDraft{..}) = Ex.withHandler draftModifyHandler $
                                                withTransaction' $ do
    eithDraft <- editDraftN x
    case eithDraft of
        Left err -> CMC.throwM $ DModifyError err
        Right draft ->
            case _ed_tags of
                Nothing -> return $ RCreated EDraft draft
                Just tags -> do
                    eithTs <- attachTagsToDraft draft tags
                    case eithTs of
                        Left err -> CMC.throwM $ DTagsError err
                        Right ts -> do
                            logInfo $ attached "draft" ts draft
                            tsRem <- removeAllButGivenTagsDraft draft tags
                            logInfo $ removed "draft" tsRem draft
                            return $ RCreated EDraft draft
         
            --return (ok "Draft successfully edited" Ae.Null)


publishHandler :: MonadNews m => DraftModifyError -> m APIResult
publishHandler = draftModifyHandler

publish :: (MonadNews m) => WithAuthor Publish -> m APIResult
publish x@(WithAuthor a Publish{..}) = Ex.withHandler publishHandler $
                                       withTransaction' $ do
    eithDraft <- getDraftRaw x
    case eithDraft of
        Nothing -> return $ RNotFound EDraft
        Just draft ->
            case _dr_postId draft of
                Nothing -> publishCreate draft
                Just post -> publishEdit post draft


publishCreate :: (MonadNews m) => DraftRaw -> m APIResult
publishCreate x = do
    eithPost <- createPost x
    case eithPost of
        Left err -> CMC.throwM $ DModifyError err
        Right post -> do
            logInfo $ "Created post with id = " <> showText post
            eithDraft <- editDraftPublish (EditDraftPublish post $ _dr_draftId x)
            case eithDraft of
                Left err -> CMC.throwM $ DModifyError err
                Right draft -> do
                    logInfo $ "Added post_id to draft with id = " <> showText draft
                    eithTags_ <- attachTagsToPost post (_dr_tagIds x)
                    case eithTags_ of
                        Left err -> CMC.throwM $ DTagsError err
                        Right tags_ -> do
                            logInfo $ attached "post" tags_ post
                            return $ RCreated EPost post
                            --return $ okCreated "Post successfully created" post


publishEdit :: (MonadNews m) => Int -> DraftRaw -> m APIResult
publishEdit post draft = Ex.withHandler draftModifyHandler $ withTransaction' $ do
    let
        publishPost = func post draft
    eithPost_ <- editPostPublish publishPost
    post_ <- th DModifyError eithPost_
    eithTs <- attachTagsToPost post $ _dr_tagIds draft
    ts <- th DTagsError eithTs
    logInfo $ attached "post" ts post
    tsRem <- removeAllButGivenTagsPost post $ _dr_tagIds draft
    logInfo $ removed "post" tsRem draft
    --return $ okCreated "Draft successfully published, post_id is in \"result\"" post
    return $ REdited EPost post_
{-
-}
th :: (CMC.MonadThrow m) => (e -> DraftModifyError) -> Either e a -> m a
th f x = either (CMC.throwM . f) return x

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



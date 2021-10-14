{-# LANGUAGE TypeFamilies,
             RecordWildCards #-}
module Execute.Draft where


import qualified Data.Text as T (pack, Text)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Database.PostgreSQL.Simple as PS
import qualified App.Database as D
import Database.SqlValue
import Database
import Result
import qualified Exceptions as Ex
import qualified Data.Aeson as Ae (Value(..))
import Types
import Execute.Utils
import qualified Control.Monad.Catch as CMC

draftModifyErrorToApiResult :: DraftModifyError -> APIResult
draftModifyErrorToApiResult (DModifyError x) = modifyErrorToApiResult EDraft x
draftModifyErrorToApiResult (DTagsError (TagsAttachError (ForeignViolation field value))) = RInvalidTag value

draftModifyHandler :: (CMC.MonadCatch m) => DraftModifyError -> m APIResult
draftModifyHandler = return . draftModifyErrorToApiResult

createDraft :: (CMC.MonadCatch m) => D.Handle m -> WithAuthor CreateDraft -> m APIResult
createDraft h x@(WithAuthor a CreateDraft{..}) =
  Ex.withHandler draftModifyHandler $ D.withTransaction h $ do
    eithDraft <- D.createDraft h (D.log h) x
    case eithDraft of
        Left err -> CMC.throwM $ DModifyError err
        Right draft -> do
            D.logDebug h $ "Created draft with id = " <> (T.pack $ show draft)
            eithTags <- D.attachTagsToDraft h (D.log h) draft _cd_tags
            case eithTags of
              Left err -> CMC.throwM $ DTagsError err 
              Right tags -> do
                D.logInfo h $ attached "draft" tags draft
                return $ RCreated EDraft draft

editDraft :: (CMC.MonadCatch m) => D.Handle m -> WithAuthor EditDraft -> m APIResult
editDraft
    h@(D.Handle { withTransaction = withTr })
    x@(WithAuthor a EditDraft{..}) =
        Ex.withHandler draftModifyHandler $
            withTr $ do
    eithDraft <- D.editDraft h (D.log h) x
    case eithDraft of
        Left err -> CMC.throwM $ DModifyError err
        Right draft ->
            case _ed_tags of
                Nothing -> return $ RCreated EDraft draft
                Just tags -> do
                    eithTs <- D.attachTagsToDraft h (D.log h) draft tags
                    case eithTs of
                        Left err -> CMC.throwM $ DTagsError err
                        Right ts -> do
                            D.logInfo h $ attached "draft" ts draft
                            tsRem <- D.removeAllButGivenTagsDraft h (D.log h) draft tags
                            D.logInfo h $ removed "draft" tsRem draft
                            return $ RCreated EDraft draft
         


publishHandler :: CMC.MonadCatch m => DraftModifyError -> m APIResult
publishHandler = draftModifyHandler

publish :: (CMC.MonadCatch m) => D.Handle m -> WithAuthor Publish -> m APIResult
publish h x@(WithAuthor a Publish{..}) = Ex.withHandler publishHandler $
                                       D.withTransaction h $ do
    eithDraft <- D.getDraftRaw h (D.log h) x
    case eithDraft of
        Nothing -> return $ RNotFound EDraft
        Just draft ->
            case _dr_postId draft of
                Nothing -> publishCreate h draft
                Just post -> publishEdit h post draft


publishCreate :: (CMC.MonadCatch m) => D.Handle m -> DraftRaw -> m APIResult
publishCreate h x = do
    eithPost <- D.createPost h (D.log h) x
    case eithPost of
        Left err -> CMC.throwM $ DModifyError err
        Right post -> do
            D.logInfo h $ "Created post with id = " <> showText post
            eithDraft <- D.editDraftPublish h (D.log h) (EditDraftPublish post $ _dr_draftId x)
            case eithDraft of
                Left err -> CMC.throwM $ DModifyError err
                Right draft -> do
                    D.logInfo h $ "Added post_id to draft with id = " <> showText draft
                    eithTags_ <- D.attachTagsToPost h (D.log h) post (_dr_tagIds x)
                    case eithTags_ of
                        Left err -> CMC.throwM $ DTagsError err
                        Right tags_ -> do
                            D.logInfo h $ attached "post" tags_ post
                            return $ RCreated EPost post


publishEdit :: (CMC.MonadCatch m) => D.Handle m -> Int -> DraftRaw -> m APIResult
publishEdit h post draft = Ex.withHandler draftModifyHandler $ D.withTransaction h $ do
    let
        publishPost = func post draft
    eithPost_ <- D.editPostPublish h (D.log h) publishPost
    post_ <- th DModifyError eithPost_
    eithTs <- D.attachTagsToPost h (D.log h) post $ _dr_tagIds draft
    ts <- th DTagsError eithTs
    D.logInfo h $ attached "post" ts post
    tsRem <- D.removeAllButGivenTagsPost h (D.log h) post $ _dr_tagIds draft
    D.logInfo h $ removed "post" tsRem draft
    return $ REdited EPost post_


th :: (CMC.MonadCatch m) => (e -> DraftModifyError) -> Either e a -> m a
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



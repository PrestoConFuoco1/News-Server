{-# LANGUAGE TypeFamilies, RecordWildCards #-}

module Execute.Draft
    ( createDraft
    , editDraft
    , publish
    ) where

import qualified App.Database as D
import qualified App.Logger as L
import qualified Control.Monad.Catch as CMC
import qualified Data.Text as Text (Text, pack)
import qualified Exceptions as Ex
import qualified Types as T
import qualified Utils as S

draftModifyErrorToApiResult :: T.DraftModifyError -> T.APIResult
draftModifyErrorToApiResult (T.DModifyError x) = T.RFailed T.EDraft x
draftModifyErrorToApiResult (T.DTagsError (T.TagsAttachError T.ForeignViolation {..})) =
    T.RInvalidTag _fv_value

--    T.RFailed T.EDraft ?
draftModifyHandler ::
       (CMC.MonadCatch m)
    => L.LoggerHandler m
    -> T.DraftModifyError
    -> m T.APIResult
draftModifyHandler logger err = do
    L.logError logger "Error occured, all changes are discarded"
    L.logError logger $ Text.pack $ CMC.displayException err
    pure $ draftModifyErrorToApiResult err

createDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WithAuthor T.CreateDraft
    -> m T.APIResult
createDraft h x@(T.WithAuthor _ T.CreateDraft {..}) =
    Ex.withHandler (draftModifyHandler $ D.log h) $
    D.withTransaction h $ do
        eithDraft <- D.createDraft h (D.log h) x
        draft <- throwWithFuncOnError T.DModifyError eithDraft
        D.logInfo h $
            "Created draft with id = " <> Text.pack (show draft)
        eithTags <- D.attachTagsToDraft h (D.log h) draft _cd_tags
        tags <- throwWithFuncOnError T.DTagsError eithTags
        D.logInfo h $ attached "draft" tags draft
        pure $ T.RCreated T.EDraft draft

editDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WithAuthor T.EditDraft
    -> m T.APIResult
editDraft h x@(T.WithAuthor _ T.EditDraft {..}) =
    Ex.withHandler (draftModifyHandler $ D.log h) $
    D.withTransaction h $ do
        eithDraft <- D.editDraft h (D.log h) x
        draft <- throwWithFuncOnError T.DModifyError eithDraft
        S.withMaybe _ed_tags (pure $ T.REdited T.EDraft draft) $ \tags -> do
            eithTs <- D.attachTagsToDraft h (D.log h) draft tags
            ts <- throwWithFuncOnError T.DTagsError eithTs
            D.logInfo h $ attached "draft" ts draft
            tsRem <-
                D.removeAllButGivenTagsDraft h (D.log h) draft tags
            D.logInfo h $ removed "draft" tsRem draft
            pure $ T.REdited T.EDraft draft

publishHandler ::
       CMC.MonadCatch m
    => L.LoggerHandler m
    -> T.DraftModifyError
    -> m T.APIResult
publishHandler = draftModifyHandler

publish ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WithAuthor T.Publish
    -> m T.APIResult
publish h x@(T.WithAuthor _ T.Publish {}) =
    Ex.withHandler (publishHandler $ D.log h) $
    D.withTransaction h $ do
        eithDraft <- D.getDraftRaw h (D.log h) x
        case eithDraft of
            Nothing -> pure $ T.RFailed T.EDraft T.MNoAction
            Just draft ->
                case T._dr_postId draft of
                    Nothing -> publishCreate h draft
                    Just post -> publishEdit h post draft

publishCreate ::
       (CMC.MonadCatch m) => D.Handle m -> T.DraftRaw -> m T.APIResult
publishCreate h x = do
    eithPost <- D.createPost h (D.log h) x
    post <- throwWithFuncOnError T.DModifyError eithPost
    D.logInfo h $ "Created post with id = " <> S.showText post
    eithDraft <-
        D.editDraftPublish
            h
            (D.log h)
            (T.EditDraftPublish post $ T._dr_draftId x)
    draft <- throwWithFuncOnError T.DModifyError eithDraft
    D.logInfo h $
        "Added post_id to draft with id = " <> S.showText draft
    eithTags_ <- D.attachTagsToPost h (D.log h) post (T._dr_tagIds x)
    tags_ <- throwWithFuncOnError T.DTagsError eithTags_
    D.logInfo h $ attached "post" tags_ post
    pure $ T.RCreated T.EPost post

publishEdit ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> Int
    -> T.DraftRaw
    -> m T.APIResult
publishEdit h post draft =
    Ex.withHandler (draftModifyHandler $ D.log h) $
    D.withTransaction h $ do
        let publishPost = draftRawToPublishEditPost post draft
        eithPost_ <- D.editPostPublish h (D.log h) publishPost
        post_ <- throwWithFuncOnError T.DModifyError eithPost_
        eithTs <-
            D.attachTagsToPost h (D.log h) post $ T._dr_tagIds draft
        ts <- throwWithFuncOnError T.DTagsError eithTs
        D.logInfo h $ attached "post" ts post
        tsRem <-
            D.removeAllButGivenTagsPost h (D.log h) post $
            T._dr_tagIds draft
        D.logInfo h $ removed "post" tsRem $ T._dr_draftId draft
        pure $ T.REdited T.EPost post_

throwWithFuncOnError ::
       (CMC.MonadCatch m)
    => (e -> T.DraftModifyError)
    -> Either e a
    -> m a
throwWithFuncOnError f = either (CMC.throwM . f) pure

draftRawToPublishEditPost :: Int -> T.DraftRaw -> T.PublishEditPost
draftRawToPublishEditPost post T.DraftRaw {..} =
    let _pep_postId = post
        _pep_title = _dr_title
        _pep_categoryId = _dr_categoryId
        _pep_content = _dr_content
        _pep_mainPhoto = _dr_mainPhoto
        _pep_extraPhotos = _dr_extraPhotos
     in T.PublishEditPost {..}

attached, removed :: Text.Text -> [Int] -> Int -> Text.Text
attached ent ids eid =
    "Attached tags with id in " <>
    S.showText ids <> " to " <> ent <> " with id = " <> S.showText eid

removed ent ids eid =
    "Removed tags with id in " <>
    S.showText ids <> " to " <> ent <> " with id = " <> S.showText eid

{-# LANGUAGE TypeFamilies, RecordWildCards #-}

module Execute.Draft
    ( createDraft
    , editDraft
    , publish
    ) where

import qualified App.Database as D
import qualified App.Logger as L
import qualified Control.Monad.Catch as CMC
import qualified Data.Text as T (Text, pack)
import qualified Exceptions as Ex
import Execute.Utils (modifyErrorToApiResult)
import qualified Types as Y
import qualified Utils as S

draftModifyErrorToApiResult :: Y.DraftModifyError -> Y.APIResult
draftModifyErrorToApiResult (Y.DModifyError x) =
    modifyErrorToApiResult Y.EDraft x
draftModifyErrorToApiResult (Y.DTagsError (Y.TagsAttachError (Y.ForeignViolation _ value))) =
    Y.RInvalidTag value

draftModifyHandler ::
       (CMC.MonadCatch m)
    => L.LoggerHandler m
    -> Y.DraftModifyError
    -> m Y.APIResult
draftModifyHandler logger err = do
    L.logError logger "Error occured, all changes are discarded"
    L.logError logger $ T.pack $ CMC.displayException err
    pure $ draftModifyErrorToApiResult err

createDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> Y.WithAuthor Y.CreateDraft
    -> m Y.APIResult
createDraft h x@(Y.WithAuthor _ Y.CreateDraft {..}) =
    Ex.withHandler (draftModifyHandler $ D.log h) $
    D.withTransaction h $ do
        eithDraft <- D.createDraft h (D.log h) x
        draft <- throwWithFunc Y.DModifyError eithDraft
        D.logInfo h $
            "Created draft with id = " <> T.pack (show draft)
        eithTags <- D.attachTagsToDraft h (D.log h) draft _cd_tags
        tags <- throwWithFunc Y.DTagsError eithTags
        D.logInfo h $ attached "draft" tags draft
        pure $ Y.RCreated Y.EDraft draft

editDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> Y.WithAuthor Y.EditDraft
    -> m Y.APIResult
editDraft h x@(Y.WithAuthor _ Y.EditDraft {..}) =
    Ex.withHandler (draftModifyHandler $ D.log h) $
    D.withTransaction h $ do
        eithDraft <- D.editDraft h (D.log h) x
        draft <- throwWithFunc Y.DModifyError eithDraft
        S.withMaybe _ed_tags (pure $ Y.REdited Y.EDraft draft) $ \tags -> do
            eithTs <- D.attachTagsToDraft h (D.log h) draft tags
            ts <- throwWithFunc Y.DTagsError eithTs
            D.logInfo h $ attached "draft" ts draft
            tsRem <-
                D.removeAllButGivenTagsDraft h (D.log h) draft tags
            D.logInfo h $ removed "draft" tsRem draft
            pure $ Y.REdited Y.EDraft draft

publishHandler ::
       CMC.MonadCatch m
    => L.LoggerHandler m
    -> Y.DraftModifyError
    -> m Y.APIResult
publishHandler = draftModifyHandler

publish ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> Y.WithAuthor Y.Publish
    -> m Y.APIResult
publish h x@(Y.WithAuthor _ Y.Publish {}) =
    Ex.withHandler (publishHandler $ D.log h) $
    D.withTransaction h $ do
        eithDraft <- D.getDraftRaw h (D.log h) x
        case eithDraft of
            Nothing -> pure $ Y.RNotFound Y.EDraft
            Just draft ->
                case Y._dr_postId draft of
                    Nothing -> publishCreate h draft
                    Just post -> publishEdit h post draft

publishCreate ::
       (CMC.MonadCatch m) => D.Handle m -> Y.DraftRaw -> m Y.APIResult
publishCreate h x = do
    eithPost <- D.createPost h (D.log h) x
    post <- throwWithFunc Y.DModifyError eithPost
    D.logInfo h $ "Created post with id = " <> showText post
    eithDraft <-
        D.editDraftPublish
            h
            (D.log h)
            (Y.EditDraftPublish post $ Y._dr_draftId x)
    draft <- throwWithFunc Y.DModifyError eithDraft
    D.logInfo h $
        "Added post_id to draft with id = " <> showText draft
    eithTags_ <- D.attachTagsToPost h (D.log h) post (Y._dr_tagIds x)
    tags_ <- throwWithFunc Y.DTagsError eithTags_
    D.logInfo h $ attached "post" tags_ post
    pure $ Y.RCreated Y.EPost post

publishEdit ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> Int
    -> Y.DraftRaw
    -> m Y.APIResult
publishEdit h post draft =
    Ex.withHandler (draftModifyHandler $ D.log h) $
    D.withTransaction h $ do
        let publishPost = draftRawToPublishEditPost post draft
        eithPost_ <- D.editPostPublish h (D.log h) publishPost
        post_ <- throwWithFunc Y.DModifyError eithPost_
        eithTs <-
            D.attachTagsToPost h (D.log h) post $ Y._dr_tagIds draft
        ts <- throwWithFunc Y.DTagsError eithTs
        D.logInfo h $ attached "post" ts post
        tsRem <-
            D.removeAllButGivenTagsPost h (D.log h) post $
            Y._dr_tagIds draft
        D.logInfo h $ removed "post" tsRem $ Y._dr_draftId draft
        pure $ Y.REdited Y.EPost post_

throwWithFunc ::
       (CMC.MonadCatch m)
    => (e -> Y.DraftModifyError)
    -> Either e a
    -> m a
throwWithFunc f = either (CMC.throwM . f) pure

draftRawToPublishEditPost :: Int -> Y.DraftRaw -> Y.PublishEditPost
draftRawToPublishEditPost post Y.DraftRaw {..} =
    let _pep_postId = post
        _pep_title = _dr_title
        _pep_categoryId = _dr_categoryId
        _pep_content = _dr_content
        _pep_mainPhoto = _dr_mainPhoto
        _pep_extraPhotos = _dr_extraPhotos
     in Y.PublishEditPost {..}

showText :: (Show a) => a -> T.Text
showText = T.pack . show

attached, removed :: T.Text -> [Int] -> Int -> T.Text
attached ent ids eid =
    "Attached tags with id in " <>
    showText ids <> " to " <> ent <> " with id = " <> showText eid

removed ent ids eid =
    "Removed tags with id in " <>
    showText ids <> " to " <> ent <> " with id = " <> showText eid

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

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
draftModifyErrorToApiResult (T.DraftModifyError x) =
    T.RFailed T.EDraft x
draftModifyErrorToApiResult (T.DraftTagsError (T.TagsAttachError T.ForeignViolation {..})) =
    T.RInvalidTag fvValue

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
    => D.DraftsHandler m
    -> L.LoggerHandler m
    -> T.WithAuthor T.CreateDraft
    -> m T.APIResult
createDraft draftsH logger x@(T.WithAuthor _ T.CreateDraft {..}) =
    Ex.withHandler (draftModifyHandler logger) $
    D.withTransaction draftsH $ do
        eithDraft <- D.createDraft draftsH logger x
        draft <- throwWithFuncOnError T.DraftModifyError eithDraft
        L.logInfo logger $
            "Created draft with id = " <> Text.pack (show draft)
        eithTags <- D.attachTagsToDraft draftsH logger draft cdTags
        tags <- throwWithFuncOnError T.DraftTagsError eithTags
        L.logInfo logger $ attached "draft" tags draft
        pure $ T.RCreated T.EDraft draft

editDraft ::
       (CMC.MonadCatch m)
    => D.DraftsHandler m
    -> L.LoggerHandler m
    -> T.WithAuthor T.EditDraft
    -> m T.APIResult
editDraft draftsH logger x@(T.WithAuthor _ T.EditDraft {..}) =
    Ex.withHandler (draftModifyHandler logger) $
    D.withTransaction draftsH $ do
        eithDraft <- D.editDraft draftsH logger x
        draft <- throwWithFuncOnError T.DraftModifyError eithDraft
        S.withMaybe edTags (pure $ T.REdited T.EDraft draft) $ \tags -> do
            eithTs <- D.attachTagsToDraft draftsH logger draft tags
            ts <- throwWithFuncOnError T.DraftTagsError eithTs
            L.logInfo logger $ attached "draft" ts draft
            tsRem <-
                D.removeAllButGivenTagsDraft draftsH logger draft tags
            L.logInfo logger $ removed "draft" tsRem draft
            pure $ T.REdited T.EDraft draft

publishHandler ::
       CMC.MonadCatch m
    => L.LoggerHandler m
    -> T.DraftModifyError
    -> m T.APIResult
publishHandler = draftModifyHandler

publish ::
       (CMC.MonadCatch m)
    => D.DraftsHandler m
    -> L.LoggerHandler m
    -> T.WithAuthor T.Publish
    -> m T.APIResult
publish draftsH logger x@(T.WithAuthor _ T.Publish {}) =
    Ex.withHandler (publishHandler logger) $
    D.withTransaction draftsH $ do
        eithDraft <- D.getDraftRaw draftsH logger x
        case eithDraft of
            Nothing -> pure $ T.RFailed T.EDraft T.MNoAction
            Just draft ->
                case T.drPostId draft of
                    Nothing -> publishCreate draftsH logger draft
                    Just post -> publishEdit draftsH logger post draft

publishCreate ::
       (CMC.MonadCatch m)
    => D.DraftsHandler m
    -> L.LoggerHandler m
    -> T.DraftRaw
    -> m T.APIResult
publishCreate draftsH logger x = do
    eithPost <- D.createPost draftsH logger x
    post <- throwWithFuncOnError T.DraftModifyError eithPost
    L.logInfo logger $ "Created post with id = " <> S.showText post
    eithDraft <-
        D.editDraftPublish
            draftsH
            logger
            (T.EditDraftPublish post $ T.drDraftId x)
    draft <- throwWithFuncOnError T.DraftModifyError eithDraft
    L.logInfo logger $
        "Added post_id to draft with id = " <> S.showText draft
    eithTags_ <- D.attachTagsToPost draftsH logger post (T.drTagIds x)
    tags_ <- throwWithFuncOnError T.DraftTagsError eithTags_
    L.logInfo logger $ attached "post" tags_ post
    pure $ T.RCreated T.EPost post

publishEdit ::
       (CMC.MonadCatch m)
    => D.DraftsHandler m
    -> L.LoggerHandler m
    -> Int
    -> T.DraftRaw
    -> m T.APIResult
publishEdit draftsH logger post draft =
    Ex.withHandler (draftModifyHandler logger) $
    D.withTransaction draftsH $ do
        let publishPost = draftRawToPublishEditPost post draft
        eithPost_ <- D.editPostPublish draftsH logger publishPost
        post_ <- throwWithFuncOnError T.DraftModifyError eithPost_
        eithTs <-
            D.attachTagsToPost draftsH logger post $ T.drTagIds draft
        ts <- throwWithFuncOnError T.DraftTagsError eithTs
        L.logInfo logger $ attached "post" ts post
        tsRem <-
            D.removeAllButGivenTagsPost draftsH logger post $
            T.drTagIds draft
        L.logInfo logger $ removed "post" tsRem $ T.drDraftId draft
        pure $ T.REdited T.EPost post_

throwWithFuncOnError ::
       (CMC.MonadCatch m)
    => (e -> T.DraftModifyError)
    -> Either e a
    -> m a
throwWithFuncOnError f = either (CMC.throwM . f) pure

draftRawToPublishEditPost :: Int -> T.DraftRaw -> T.PublishEditPost
draftRawToPublishEditPost post T.DraftRaw {..} =
    let pepPostId = post
        pepTitle = drTitle
        pepCategoryId = drCategoryId
        pepContent = drContent
        pepMainPhoto = drMainPhoto
        pepExtraPhotos = drExtraPhotos
     in T.PublishEditPost {..}

attached, removed :: Text.Text -> [Int] -> Int -> Text.Text
attached ent ids eid =
    "Attached tags with id in " <>
    S.showText ids <> " to " <> ent <> " with id = " <> S.showText eid

removed ent ids eid =
    "Removed tags with id in " <>
    S.showText ids <> " to " <> ent <> " with id = " <> S.showText eid

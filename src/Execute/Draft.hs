{-# LANGUAGE TypeFamilies, RecordWildCards #-}

module Execute.Draft where

import qualified App.Database as D
import qualified App.Logger as L
import qualified Control.Monad.Catch as CMC
import qualified Data.Text as T (Text, pack)
import qualified Exceptions as Ex
import Execute.Utils (modifyErrorToApiResult)
import Types
import qualified Utils as S

draftModifyErrorToApiResult :: DraftModifyError -> APIResult
draftModifyErrorToApiResult (DModifyError x) =
   modifyErrorToApiResult EDraft x
draftModifyErrorToApiResult (DTagsError (TagsAttachError (ForeignViolation _ value))) =
   RInvalidTag value

draftModifyHandler ::
      (CMC.MonadCatch m)
   => L.Handle m
   -> DraftModifyError
   -> m APIResult
draftModifyHandler logger err = do
   L.logError
      logger
      "Error occured, all changes are discarded"
   L.logError logger $ T.pack $ CMC.displayException err
   pure $ draftModifyErrorToApiResult err

createDraft ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> WithAuthor CreateDraft
   -> m APIResult
createDraft h x@(WithAuthor _ CreateDraft {..}) =
   Ex.withHandler (draftModifyHandler $ D.log h) $
   D.withTransaction h $ do
      eithDraft <- D.createDraft h (D.log h) x
      draft <- th DModifyError eithDraft
      D.logInfo h $
         "Created draft with id = " <> T.pack (show draft)
      eithTags <-
         D.attachTagsToDraft h (D.log h) draft _cd_tags
      tags <- th DTagsError eithTags
      D.logInfo h $ attached "draft" tags draft
      pure $ RCreated EDraft draft

editDraft ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> WithAuthor EditDraft
   -> m APIResult
editDraft h x@(WithAuthor _ EditDraft {..}) =
   Ex.withHandler (draftModifyHandler $ D.log h) $
   D.withTransaction h $ do
      eithDraft <- D.editDraft h (D.log h) x
      draft <- th DModifyError eithDraft
      S.withMaybe _ed_tags (pure $ REdited EDraft draft) $ \tags -> do
         eithTs <-
            D.attachTagsToDraft h (D.log h) draft tags
         ts <- th DTagsError eithTs
         D.logInfo h $ attached "draft" ts draft
         tsRem <-
            D.removeAllButGivenTagsDraft
               h
               (D.log h)
               draft
               tags
         D.logInfo h $ removed "draft" tsRem draft
         pure $ REdited EDraft draft

publishHandler ::
      CMC.MonadCatch m
   => L.Handle m
   -> DraftModifyError
   -> m APIResult
publishHandler = draftModifyHandler

publish ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> WithAuthor Publish
   -> m APIResult
publish h x@(WithAuthor _ Publish {}) =
   Ex.withHandler (publishHandler $ D.log h) $
   D.withTransaction h $ do
      eithDraft <- D.getDraftRaw h (D.log h) x
      case eithDraft of
         Nothing -> pure $ RNotFound EDraft
         Just draft ->
            case _dr_postId draft of
               Nothing -> publishCreate h draft
               Just post -> publishEdit h post draft

publishCreate ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> DraftRaw
   -> m APIResult
publishCreate h x = do
   eithPost <- D.createPost h (D.log h) x
   post <- th DModifyError eithPost
   D.logInfo h $ "Created post with id = " <> showText post
   eithDraft <-
      D.editDraftPublish
         h
         (D.log h)
         (EditDraftPublish post $ _dr_draftId x)
   draft <- th DModifyError eithDraft
   D.logInfo h $
      "Added post_id to draft with id = " <> showText draft
   eithTags_ <-
      D.attachTagsToPost h (D.log h) post (_dr_tagIds x)
   tags_ <- th DTagsError eithTags_
   D.logInfo h $ attached "post" tags_ post
   pure $ RCreated EPost post

publishEdit ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> Int
   -> DraftRaw
   -> m APIResult
publishEdit h post draft =
   Ex.withHandler (draftModifyHandler $ D.log h) $
   D.withTransaction h $ do
      let publishPost = func post draft
      eithPost_ <- D.editPostPublish h (D.log h) publishPost
      post_ <- th DModifyError eithPost_
      eithTs <-
         D.attachTagsToPost h (D.log h) post $
         _dr_tagIds draft
      ts <- th DTagsError eithTs
      D.logInfo h $ attached "post" ts post
      tsRem <-
         D.removeAllButGivenTagsPost h (D.log h) post $
         _dr_tagIds draft
      D.logInfo h $ removed "post" tsRem $ _dr_draftId draft
      pure $ REdited EPost post_

th :: (CMC.MonadCatch m)
   => (e -> DraftModifyError)
   -> Either e a
   -> m a
th f = either (CMC.throwM . f) pure

func :: Int -> DraftRaw -> PublishEditPost
func post DraftRaw {..} =
   let _pep_postId = post
       _pep_title = _dr_title
       _pep_categoryId = _dr_categoryId
       _pep_content = _dr_content
       _pep_mainPhoto = _dr_mainPhoto
       _pep_extraPhotos = _dr_extraPhotos
    in PublishEditPost {..}

showText :: (Show a) => a -> T.Text
showText = T.pack . show

attached, removed :: T.Text -> [Int] -> Int -> T.Text
attached ent ids eid =
   "Attached tags with id in " <>
   showText ids <>
   " to " <> ent <> " with id = " <> showText eid

removed ent ids eid =
   "Removed tags with id in " <>
   showText ids <>
   " to " <> ent <> " with id = " <> showText eid

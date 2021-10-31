module Action.Draft
    ( createDraftToAction
    , editDraftToAction
    , deleteDraftToAction
    , publishAction
    ) where

import Action.Common (Router)
import qualified Action.Utils as AU
import Prelude hiding (readList)
import qualified Types as T

createDraftToAction :: Router T.CreateDraft
createDraftToAction = do
    title <- AU.requireField AU.validateNotEmpty "title"
    tags <- AU.requireField AU.readList "tags"
    category <- AU.requireField AU.readInt "category_id"
    content <- AU.requireField AU.validateNotEmpty "content"
    mainPhoto <- AU.optional AU.validateNotEmpty "main_photo"
    extraPhotos <- AU.optional AU.readList "extra_photos"
    pure $
        T.CreateDraft
            title
            tags
            category
            content
            mainPhoto
            extraPhotos

editDraftToAction :: Router T.EditDraft
editDraftToAction = do
    draftId <- AU.requireField AU.readInt "draft_id"
    title <- AU.optional AU.validateNotEmpty "title"
    tags <- AU.optional AU.readList "tags"
    category <- AU.optional AU.readInt "category_id"
    content <- AU.optional AU.validateNotEmpty "content"
    mainPhoto <- AU.optional AU.validateNotEmpty "main_photo"
    extraPhotos <- AU.optional AU.readList "extra_photos"
    pure $
        T.EditDraft
            draftId
            title
            tags
            category
            content
            mainPhoto
            extraPhotos

deleteDraftToAction :: Router T.DeleteDraft
deleteDraftToAction = do
    draftId <- AU.requireField AU.readInt "draft_id"
    pure $ T.DeleteDraft draftId

publishAction :: Router T.Publish
publishAction = do
    draftId <- AU.requireField AU.readInt "draft_id"
    pure $ T.Publish draftId

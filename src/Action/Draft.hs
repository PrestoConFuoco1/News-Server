module Action.Draft where

import Action.Common (Router)
import qualified Action.Utils as AU
import Prelude hiding (readList)
import qualified Types as Y

createDraftToAction :: Router Y.CreateDraft
createDraftToAction = do
   title <- AU.requireField AU.validateNotEmpty "title"
   tags <- AU.requireField AU.readList "tags"
   category <- AU.requireField AU.readInt "category_id"
   content <- AU.requireField AU.validateNotEmpty "content"
   mainPhoto <- AU.optional AU.validateNotEmpty "main_photo"
   extraPhotos <- AU.optional AU.readList "extra_photos"
   pure $
      Y.CreateDraft
         title
         tags
         category
         content
         mainPhoto
         extraPhotos

editDraftToAction :: Router Y.EditDraft
editDraftToAction = do
   draftId <- AU.requireField AU.readInt "draft_id"
   title <- AU.optional AU.validateNotEmpty "title"
   tags <- AU.optional AU.readList "tags"
   category <- AU.optional AU.readInt "category_id"
   content <- AU.optional AU.validateNotEmpty "content"
   mainPhoto <- AU.optional AU.validateNotEmpty "main_photo"
   extraPhotos <- AU.optional AU.readList "extra_photos"
   pure $
      Y.EditDraft
         draftId
         title
         tags
         category
         content
         mainPhoto
         extraPhotos

deleteDraftToAction :: Router Y.DeleteDraft
deleteDraftToAction = do
   draftId <- AU.requireField AU.readInt "draft_id"
   pure $ Y.DeleteDraft draftId

publishAction :: Router Y.Publish
publishAction = do
   draftId <- AU.requireField AU.readInt "draft_id"
   pure $ Y.Publish draftId

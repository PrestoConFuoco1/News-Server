module Action.Draft where

import Action.Common (Router)
import qualified Action.Utils as AU
import Prelude hiding (readList)
import Types

createDraftToAction :: Router CreateDraft
createDraftToAction = do
   title <- AU.requireField AU.validateNotEmpty "title"
   tags <- AU.requireField AU.readList "tags"
   category <- AU.requireField AU.readInt "category_id"
   content <- AU.requireField AU.validateNotEmpty "content"
   mainPhoto <- AU.optional AU.validateNotEmpty "main_photo"
   extraPhotos <- AU.optional AU.readList "extra_photos"
   pure $
      CreateDraft
         title
         tags
         category
         content
         mainPhoto
         extraPhotos

editDraftToAction :: Router EditDraft
editDraftToAction = do
   draftId <- AU.requireField AU.readInt "draft_id"
   title <- AU.optional AU.validateNotEmpty "title"
   tags <- AU.optional AU.readList "tags"
   category <- AU.optional AU.readInt "category_id"
   content <- AU.optional AU.validateNotEmpty "content"
   mainPhoto <- AU.optional AU.validateNotEmpty "main_photo"
   extraPhotos <- AU.optional AU.readList "extra_photos"
   pure $
      EditDraft
         draftId
         title
         tags
         category
         content
         mainPhoto
         extraPhotos

deleteDraftToAction :: Router DeleteDraft
deleteDraftToAction = do
   draftId <- AU.requireField AU.readInt "draft_id"
   pure $ DeleteDraft draftId

publishAction :: Router Publish
publishAction = do
   draftId <- AU.requireField AU.readInt "draft_id"
   pure $ Publish draftId

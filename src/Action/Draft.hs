module Action.Draft where

import Action.Common
import Action.Utils
import Prelude hiding (readList)
import Types

createDraftToAction :: Router CreateDraft
createDraftToAction = do
   title <- requireField validateNotEmpty "title"
   tags <- requireField readList "tags" -- допустим ли пустой список?
   category <- requireField readInt "category_id"
   content <- requireField validateNotEmpty "content"
   mainPhoto <- optional validateNotEmpty "main_photo"
   extraPhotos <- optional readList "extra_photos"
   return $
      CreateDraft
         title
         tags
         category
         content
         mainPhoto
         extraPhotos

editDraftToAction :: Router EditDraft
editDraftToAction = do
   draftId <- requireField readInt "draft_id"
   title <- optional validateNotEmpty "title"
   tags <- optional readList "tags"
   category <- optional readInt "category_id"
   content <- optional validateNotEmpty "content"
   mainPhoto <- optional validateNotEmpty "main_photo"
   extraPhotos <- optional readList "extra_photos"
   return $
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
   draftId <- requireField readInt "draft_id"
   return $ DeleteDraft draftId

publishAction :: Router Publish
publishAction = do
   draftId <- requireField readInt "draft_id"
   return $ Publish draftId

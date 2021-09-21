module Action.Draft.Draft where

import Action.Draft.Types
import qualified Data.Text as T
import Action.Common
import Action.Utils

requestToActionDrafts :: [T.Text] -> Query -> Either ActionError ActionDrafts
requestToActionDrafts path hash = case path of
  (x:xs)
    | x == "get" -> Right $ Read GetDrafts
    | x == "create" -> fmap Create $ createDraftToAction hash
    | x == "edit" -> fmap Update $ editCatsToAction hash
    | x == "delete" -> fmap Delete $ deleteDraftToAction hash
  [] -> Left EInvalidEndpoint


createDraftToAction :: Query -> Either ActionError CreateDraft
createDraftToAction hash = do
    title <- requireField (requireText hash) "title"
    tags <- requireField (requireList hash) "tags"
    category <- requireField (requireInt hash) "category_id"
    content <- requireField (requireText hash) "content"
    let mainPhoto = requireText hash "main_photo"
        extraPhotos = requireList hash "extra_photos"
    return $ CreateDraft title tags category content mainPhoto extraPhotos

editCatsToAction :: Query -> Either ActionError EditDraft
editCatsToAction hash = do
    draftId <- requireField (requireInt hash) "draft_id"
    let title = requireText hash "title"
        tags = requireList hash "tags"
        category = requireInt hash "category_id"
        content = requireText hash "content"
        mainPhoto = requireText hash "main_photo"
        extraPhotos = requireList hash "extra_photos"
    return $ EditDraft draftId title tags category content mainPhoto extraPhotos
        

deleteDraftToAction :: Query -> Either ActionError DeleteDraft
deleteDraftToAction hash = do
    draftId <- requireField (requireInt hash) "draft_id"
    return $ DeleteDraft draftId

{-
getDraftsToAction :: Query -> Either ActionError GetDrafts
getDraftsToAction hash = 
-}

requestToActionPublish :: [T.Text] -> Query -> Either ActionError Publish
requestToActionPublish path hash = case path of
    (x:xs) -> Left EInvalidEndpoint
    [] -> publishAction hash


publishAction :: Query -> Either ActionError Publish
publishAction hash = do
    draftId <- requireField (requireInt hash) "draft_id"
    return $ Publish draftId
    

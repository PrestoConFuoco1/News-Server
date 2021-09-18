module Action.Tags.Tags where



import Action.Common
import qualified Data.Text as T
import Action.Tags.Types
import Action.Utils



requestToActionTags :: [T.Text] -> Query -> Either ActionError ActionTags
requestToActionTags path hash = case path of
  (x:xs)
    | x == "get" -> pure $ Read GetTags
    | x == "create" -> fmap Create $ createTagToAction hash
    | x == "edit" -> fmap Update $ editTagToAction hash
    | x == "delete" -> fmap Delete $ deleteTagToAction hash
  [] -> Left EInvalidEndpoint

createTagToAction :: Query -> Either ActionError CreateTag
createTagToAction hash = do
    name <- requireField (requireText hash) "name"
    return $ CreateTag name

editTagToAction :: Query -> Either ActionError EditTag
editTagToAction hash = do
    id <- requireField (requireInt hash) "tag_id"
    name <- requireField (requireText hash) "name"
    return $ EditTag id name

deleteTagToAction :: Query -> Either ActionError DeleteTag
deleteTagToAction hash = do
    id <- requireField (requireInt hash) "tag_id"
    return $ DeleteTag id








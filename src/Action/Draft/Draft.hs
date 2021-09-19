module Action.Draft.Draft where

import Action.Draft.Types
import qualified Data.Text as T
import Action.Common
import Action.Utils

requestToActionDrafts :: [T.Text] -> Query -> Either ActionError ActionDrafts
requestToActionDrafts path hash = case path of
  (x:xs)
--    | x == "get" -> Right $ Read GetCategories
    | x == "create" -> fmap Create $ createDraftToAction hash
--    | x == "edit" -> fmap Update $ editCatsToAction hash
--    | x == "delete" -> fmap Delete $ deleteCatsToAction hash
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


{-
getPostsAction :: Query -> ActionPosts
getPostsAction qu =
    let tagopts = foldr (<|>) Nothing
                           [fmap OneTag $ requireInt qu "tag",
                            fmap TagsIn $ requireIntList qu "tags__in",
                            fmap TagsAll $ requireIntList qu "tags__all"]
        creationopts = foldr (<|>) Nothing $
                       [fmap Created $ requireDay qu "created_at",
                        fmap CreatedEarlier $ requireDay qu "created_at__lt",
                        fmap CreatedLater   $ requireDay qu "created_at__gt"]
        sortopts = maybe defaultSortOptions id
                       -- $ (HS.lookup "sort" qu >>= sortOptions)
                        $ (requireByteString qu "sort" >>= sortOptions)
        searchopts = fmap SearchOptions $ requireText qu "search"
    in  Read $ GetPosts creationopts tagopts searchopts sortopts
-}


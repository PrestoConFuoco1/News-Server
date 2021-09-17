module Posts where


import Action.Common
import qualified Data.Text as T
import Action.Tags.Types
import Action.Utils
import Action.Posts.Types
import qualified Data.ByteString as BS
import Control.Applicative ((<|>))



sortOptions :: BS.ByteString -> Maybe SortOptions
sortOptions = undefined

requestToActionPosts :: [T.Text] -> Query -> Either ActionError ActionPosts
requestToActionPosts path hash = case path of
  (x:xs)
    | x == "get" -> Right $ getPostsAction hash
    | otherwise -> case readIntText x of
        (Just id) -> actionWithPost id xs hash
        Nothing -> Left EInvalidEndpoint
  [] -> Left EInvalidEndpoint

actionWithPost :: Int -> [T.Text] -> Query -> Either ActionError ActionPosts
actionWithPost id path hash = case path of
  (x:xs)
   -- | x == "comments" -> AGetComments $ GetComments id
    | x == "comments" -> undefined
  [] -> Left EInvalidEndpoint


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




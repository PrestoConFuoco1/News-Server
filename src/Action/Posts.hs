module Action.Posts where

import Action.Common
import Action.Utils
import qualified Data.ByteString as BS
import Prelude hiding (readList)
import qualified Data.Text as T
import Types

defaultSortOptions :: SortOptions
defaultSortOptions = SortOptions SEDate SODescending -- newer posts first


actionWithPost :: Int -> [T.Text] -> Query -> Either ActionErrorPerms (Paginated GetComments)
actionWithPost id path hash = case path of
  (x:[])
    | x == "comments" -> runRouter (renv False hash) $ withPagination $ return $ GetComments id
  _ -> Left $ ActionErrorPerms False EInvalidEndpoint


getPostsAction :: Router GetPosts
getPostsAction = do
    tagopts <- oneOf [fmap2 OneTag $ optional readInt "tag",
                      fmap2 TagsIn $ optional readList "tags__in",
                      fmap2 TagsAll $ optional readList "tags__all"]
 
    creationopts <- oneOf
                       [fmap2 Created $ optional readDay "created_at",
                        fmap2 CreatedEarlier $ optional readDay "created_at__lt",
                        fmap2 CreatedLater   $ optional readDay "created_at__gt"]
    sortoptsRaw <- optional validateNotEmpty "sort" :: Router (Maybe T.Text)
    sortopts <- case sortoptsRaw of
        Nothing -> return defaultSortOptions
        Just s -> errorOnNothing (EInvalidFieldValue "sort") $ sortOptions s   
    searchopts <- fmap2 SearchOptions $ optional validateNotEmpty "search"
    return $ GetPosts creationopts tagopts searchopts sortopts


sortOptions :: T.Text -> Maybe SortOptions
sortOptions text = sortOptions' $ T.unpack text


sortOptions' (x:y:_) = SortOptions <$> toSortBy x <*> ascDesc y
sortOptions' _ = Nothing

toSortBy :: Char -> Maybe SortEntity
toSortBy 'd' = Just SEDate
toSortBy 'a' = Just SEAuthor
toSortBy 'c' = Just SECategory
toSortBy 'p' = Just SEPhotoNumber
toSortBy  _  = Nothing

ascDesc :: Char -> Maybe SortOrder
ascDesc 'a' = Just SOAscending
ascDesc 'd' = Just SODescending
ascDesc _   = Nothing




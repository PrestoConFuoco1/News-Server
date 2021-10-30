module Action.Posts (
getPostsAction, actionWithPost
) where

import Action.Common
import qualified Action.Utils as AU
import qualified Data.Text as T
import Prelude hiding (readList)
import qualified Types as Y

defaultSortOptions :: Y.SortOptions
defaultSortOptions = Y.SortOptions Y.SEDate Y.SODescending

actionWithPost ::
       Int
    -> [T.Text]
    -> Query
    -> Either ActionErrorPerms (Y.Paginated Y.GetComments)
actionWithPost pid path hash =
    case path of
        [x]
            | x == "comments" ->
                runRouter (renv False hash) $
                AU.withPagination $ pure $ Y.GetComments pid
        _ -> Left $ ActionErrorPerms False EInvalidEndpoint

getPostsAction :: Router Y.GetPosts
getPostsAction = do
    tagopts <-
        AU.oneOf
            [ AU.fmap2 Y.OneTag $ AU.optional AU.readInt "tag"
            , AU.fmap2 Y.TagsIn $ AU.optional AU.readList "tags__in"
            , AU.fmap2 Y.TagsAll $ AU.optional AU.readList "tags__all"
            ]
    creationopts <-
        AU.oneOf
            [ AU.fmap2 Y.Created $ AU.optional AU.readDay "created_at"
            , AU.fmap2 Y.CreatedEarlier $
              AU.optional AU.readDay "created_at__lt"
            , AU.fmap2 Y.CreatedLater $
              AU.optional AU.readDay "created_at__gt"
            ]
    sortoptsRaw <-
        AU.optional AU.validateNotEmpty "sort" :: Router (Maybe T.Text)
    sortopts <-
        case sortoptsRaw of
            Nothing -> pure defaultSortOptions
            Just s ->
                errorOnNothing (EInvalidFieldValue "sort") $
                sortOptions s
    searchopts <-
        AU.fmap2 Y.SearchOptions $
        AU.optional AU.validateNotEmpty "search"
    pure $ Y.GetPosts creationopts tagopts searchopts sortopts

sortOptions :: T.Text -> Maybe Y.SortOptions
sortOptions text = sortOptions' $ T.unpack text

sortOptions' :: String -> Maybe Y.SortOptions
sortOptions' (x:y:_) = Y.SortOptions <$> toSortBy x <*> ascDesc y
sortOptions' _ = Nothing

toSortBy :: Char -> Maybe Y.SortEntity
toSortBy 'd' = Just Y.SEDate
toSortBy 'a' = Just Y.SEAuthor
toSortBy 'c' = Just Y.SECategory
toSortBy 'p' = Just Y.SEPhotoNumber
toSortBy _ = Nothing

ascDesc :: Char -> Maybe Y.SortOrder
ascDesc 'a' = Just Y.SOAscending
ascDesc 'd' = Just Y.SODescending
ascDesc _ = Nothing

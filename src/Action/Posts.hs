module Action.Posts
    ( getPostsAction
    , actionWithPost
    ) where

import Action.Common
import qualified Action.Utils as AU
import qualified Data.Text as Text
import Prelude hiding (readList)
import qualified Types as T

defaultSortOptions :: T.SortOptions
defaultSortOptions = T.SortOptions T.SEDate T.SODescending

actionWithPost ::
       Int
    -> [Text.Text]
    -> Query
    -> Either ActionErrorPerms (T.Paginated T.GetComments)
actionWithPost pid path hash =
    case path of
        [x]
            | x == "comments" ->
                runRouter (routingEnv False hash) $
                AU.withPagination $ pure $ T.GetComments pid
        _ -> Left $ ActionErrorPerms False EInvalidEndpoint

getPostsAction :: Router T.GetPosts
getPostsAction = do
    tagopts <-
        AU.oneOf
            [ AU.fmap2 T.OneTag $ AU.optional AU.readInt "tag"
            , AU.fmap2 T.TagsIn $ AU.optional AU.readList "tags__in"
            , AU.fmap2 T.TagsAll $ AU.optional AU.readList "tags__all"
            ]
    creationopts <-
        AU.oneOf
            [ AU.fmap2 T.Created $ AU.optional AU.readDay "created_at"
            , AU.fmap2 T.CreatedEarlier $
              AU.optional AU.readDay "created_at__lt"
            , AU.fmap2 T.CreatedLater $
              AU.optional AU.readDay "created_at__gt"
            ]
    sortoptsRaw <-
        AU.optional AU.validateNotEmpty "sort" :: Router (Maybe Text.Text)
    sortopts <-
        case sortoptsRaw of
            Nothing -> pure defaultSortOptions
            Just s ->
                errorOnNothing (EInvalidFieldValue "sort") $
                sortOptions s
    searchopts <-
        AU.fmap2 T.SearchOptions $
        AU.optional AU.validateNotEmpty "search"
    pure $ T.GetPosts creationopts tagopts searchopts sortopts

sortOptions :: Text.Text -> Maybe T.SortOptions
sortOptions text = sortOptions' $ Text.unpack text

sortOptions' :: String -> Maybe T.SortOptions
sortOptions' (x:y:_) = T.SortOptions <$> toSortBy x <*> ascDesc y
sortOptions' _ = Nothing

toSortBy :: Char -> Maybe T.SortEntity
toSortBy 'd' = Just T.SEDate
toSortBy 'a' = Just T.SEAuthor
toSortBy 'c' = Just T.SECategory
toSortBy 'p' = Just T.SEPhotoNumber
toSortBy _ = Nothing

ascDesc :: Char -> Maybe T.SortOrder
ascDesc 'a' = Just T.SOAscending
ascDesc 'd' = Just T.SODescending
ascDesc _ = Nothing

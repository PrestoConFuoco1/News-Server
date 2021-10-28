module Action.Posts where

import Action.Common
import qualified Action.Utils as AU
import qualified Data.Text as T
import Prelude hiding (readList)
import Types

defaultSortOptions :: SortOptions
defaultSortOptions = SortOptions SEDate SODescending

actionWithPost ::
      Int
   -> [T.Text]
   -> Query
   -> Either ActionErrorPerms (Paginated GetComments)
actionWithPost pid path hash =
   case path of
      [x]
         | x == "comments" ->
            runRouter (renv False hash) $
            AU.withPagination $ pure $ GetComments pid
      _ -> Left $ ActionErrorPerms False EInvalidEndpoint

getPostsAction :: Router GetPosts
getPostsAction = do
   tagopts <-
      AU.oneOf
         [ AU.fmap2 OneTag $ AU.optional AU.readInt "tag"
         , AU.fmap2 TagsIn $ AU.optional AU.readList "tags__in"
         , AU.fmap2 TagsAll $ AU.optional AU.readList "tags__all"
         ]
   creationopts <-
      AU.oneOf
         [ AU.fmap2 Created $ AU.optional AU.readDay "created_at"
         , AU.fmap2 CreatedEarlier $
           AU.optional AU.readDay "created_at__lt"
         , AU.fmap2 CreatedLater $
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
      AU.fmap2 SearchOptions $
      AU.optional AU.validateNotEmpty "search"
   pure $
      GetPosts creationopts tagopts searchopts sortopts

sortOptions :: T.Text -> Maybe SortOptions
sortOptions text = sortOptions' $ T.unpack text

sortOptions' :: String -> Maybe SortOptions
sortOptions' (x:y:_) =
   SortOptions <$> toSortBy x <*> ascDesc y
sortOptions' _ = Nothing

toSortBy :: Char -> Maybe SortEntity
toSortBy 'd' = Just SEDate
toSortBy 'a' = Just SEAuthor
toSortBy 'c' = Just SECategory
toSortBy 'p' = Just SEPhotoNumber
toSortBy _ = Nothing

ascDesc :: Char -> Maybe SortOrder
ascDesc 'a' = Just SOAscending
ascDesc 'd' = Just SODescending
ascDesc _ = Nothing

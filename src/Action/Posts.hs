{-# LANGUAGE DeriveAnyClass #-}
module Action.Posts where


import Action.Common
import Action.Utils
import qualified Data.ByteString as BS
import Prelude hiding (readList)


import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import Action.Tags
import qualified Data.Time as Time
import Data.Void
import Action.Comments

data PublishEditPost = PublishEditPost {
    _pep_postId :: Int,
    _pep_title :: T.Text,
    --_pep_creationDate :: Time.Day,
    --_pep_authorId :: Int,
    _pep_categoryId :: Int,
    _pep_content :: T.Text,
    _pep_mainPhoto :: Maybe T.Text,
    _pep_extraPhotos :: Maybe [T.Text]
    } deriving (Show, Eq)



data ActionPosts1 = AP ActionPosts | GC GetComments
    deriving (Show, Generic, GP.PrettyShow)
type ActionPosts = CRUD Void GetPosts Void Void

data GetPosts = GetPosts {
    _gp_creationDate :: Maybe CreationDateOptions,
    _gp_tags :: Maybe TagsOptions,
    _gp_search :: Maybe SearchOptions,
    _gp_sort :: SortOptions
    } deriving (Show, Generic)


data CreationDateOptions = Created Time.Day
                         | CreatedEarlier Time.Day
                         | CreatedLater Time.Day
            deriving (Show, Generic)
data TagsOptions = OneTag TagId | TagsIn [TagId] | TagsAll [TagId]
    deriving (Show, Generic)
data SearchOptions = SearchOptions T.Text
    deriving (Show, Generic)

data SortEntity = SEDate | SEAuthor | SECategory | SEPhotoNumber
    deriving (Show)
data SortOrder  = SOAscending | SODescending
    deriving (Show)
data SortOptions = SortOptions {
    _so_sortBy :: SortEntity,
    _so_order :: SortOrder
    }
    deriving (Show, Generic)


instance GP.PrettyShow CreationDateOptions where
--    prettyShow = GP.LStr . GP.gprettyShowSum . from
    prettyShow = GP.LStr . show
instance GP.PrettyShow TagsOptions where
--    prettyShow = GP.LStr . GP.gprettyShowSum . from
    prettyShow = GP.LStr . show
instance GP.PrettyShow SearchOptions where
--    prettyShow = GP.LStr . GP.gprettyShowSum . from
    prettyShow = GP.LStr . show

instance GP.PrettyShow GetPosts

instance GP.PrettyShow SortEntity where
    prettyShow = GP.LStr . drop 2 . show
instance GP.PrettyShow SortOrder where
    prettyShow = GP.LStr . drop 2 . show

instance GP.PrettyShow SortOptions where
 

defaultSortOptions :: SortOptions
defaultSortOptions = SortOptions SEDate SODescending -- newer posts first


actionWithPost :: Int -> [T.Text] -> Query -> Either ActionErrorPerms GetComments
actionWithPost id path hash = case path of
  (x:[])
    | x == "comments" -> return $ GetComments id
--    | x == "comments" -> runRouter (renv False hash) $ getCommentsToAction
  _ -> Left $ ActionErrorPerms False EInvalidEndpoint


--[Router (Maybe a)] -> Router (Maybe a)

getPostsAction :: Router ActionPosts
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
    return $ Read $ GetPosts creationopts tagopts searchopts sortopts


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




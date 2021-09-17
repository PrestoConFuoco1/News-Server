{-# LANGUAGE DeriveAnyClass #-}
module ActionTypes where

import GHC.Generics
import qualified Data.Aeson as Ae (decode, Value)

import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Aeson as Ae (decode, Value)
import qualified Data.ByteString.Lazy as BSL (fromStrict, unpack, ByteString)
import qualified Data.ByteString as BS
import qualified Data.Time as Time
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS


type PostId = Int
type TagId = Int
type CatId = Int
type Author = T.Text
--type QueryItem = (T.Text, Maybe T.Text)
--type Query = [QueryItem]
type Token = T.Text

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

defaultSortOptions :: SortOptions
defaultSortOptions = SortOptions SEDate SODescending -- newer posts first

data WhoWhat a = WhoWhat {
    _ww_token  :: Maybe Token,
    --_ww_action :: Action
    _ww_action :: a
    } deriving (Show, Generic) 

data Action = AGetPosts GetPosts

            | AGetAuthors GetAuthors

            | AGetTags GetTags
            | ACreateTag CreateTag
            | AEditTag EditTag
            | ADeleteTag DeleteTag

            | AGetComments GetComments

            | AGetCategories GetCategories
            | ACreateCategory CreateCategory
            | AEditCategory EditCategory
            | ADeleteCategory DeleteCategory

            | ACreateUser CreateUser

            | AError ActionError
    deriving (Show, Generic)

data ActionError = EInvalidEndpoint | ERequiredFieldMissing BS.ByteString
    deriving (Show, Generic)

data GetCategories = GetCategories
    deriving (Show, Generic)

data GetAuthors = GetAuthors
    deriving (Show, Generic)

data GetTags = GetTags
    deriving (Show, Generic)

data GetComments = GetComments PostId
    deriving (Show, Generic)
    

data GetPosts = GetPosts {
    _gp_creationDate :: Maybe CreationDateOptions,
    _gp_tags :: Maybe TagsOptions,
    _gp_search :: Maybe SearchOptions,
    _gp_sort :: SortOptions
    } deriving (Show, Generic)

data CreateUser = CreateUser {
    _cu_login :: T.Text,
    _cu_passHash :: T.Text,
    _cu_firstName :: T.Text,
    _cu_lastName  :: T.Text
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data CreateCategory = CreateCategory {
    _cc_catName :: T.Text,
    _cc_parentCat :: CatId
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data EditCategory = EditCategory {
    _ec_catId :: CatId,
    _ec_catName :: Maybe T.Text,
    _ec_parentId :: Maybe CatId
    } deriving (Show, Generic, GP.PrettyShow)

data DeleteCategory = DeleteCategory {
    _dc_catId :: CatId
    } deriving (Show, Generic, GP.PrettyShow)

data CreateTag = CreateTag {
    _ct_tagName :: T.Text
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data EditTag = EditTag {
    _et_tagId :: TagId,
    _et_tagName :: T.Text
    } deriving (Show, Generic, GP.PrettyShow)

data DeleteTag = DeleteTag {
    _dt_tagId :: TagId
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

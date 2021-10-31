{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Posts
    ( PostId
    , CommentId
    , PublishEditPost(..)
    , GetComments(..)
    , SearchOptions(..)
    , TagsOptions(..)
    , CreationDateOptions(..)
    , SortOptions(..)
    , SortOrder(..)
    , SortEntity(..)
    , GetPosts(..)
    , DeleteComment(..)
    , CreateComment(..)
    , ActionPosts
    , ActionPosts1(..)
    , ActionComments
    ) where

import Prelude hiding (readList)
import Types.Common (CRUD, Paginated)

import qualified Data.Text as Text
import qualified Data.Time as Time
import Data.Void
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Types.Category (CategoryId)
import Types.Tags (TagId)

type PostId = Int

type CommentId = Int

type ActionComments
     = CRUD CreateComment (Paginated GetComments) Void DeleteComment

newtype GetComments =
    GetComments
        { _gc_postId :: PostId
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (GP.PrettyShow)

data CreateComment =
    CreateComment
        { _ccom_postId :: PostId
        , _ccom_content :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

newtype DeleteComment =
    DeleteComment
        { _dc_commentId :: CommentId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data PublishEditPost =
    PublishEditPost
        { _pep_postId :: PostId
        , _pep_title :: Text.Text
        , _pep_categoryId :: CategoryId
        , _pep_content :: Text.Text
        , _pep_mainPhoto :: Maybe Text.Text
        , _pep_extraPhotos :: Maybe [Text.Text]
        }
  deriving (Show, Eq)

data ActionPosts1
    = AP ActionPosts
    | GC (Paginated GetComments)
  deriving (Show, Eq, Generic, GP.PrettyShow)

type ActionPosts = CRUD Void (Paginated GetPosts) Void Void

data GetPosts =
    GetPosts
        { _gp_creationDate :: Maybe CreationDateOptions
        , _gp_tags :: Maybe TagsOptions
        , _gp_search :: Maybe SearchOptions
        , _gp_sort :: SortOptions
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (GP.PrettyShow)

data CreationDateOptions
    = Created Time.Day
    | CreatedEarlier Time.Day
    | CreatedLater Time.Day
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable CreationDateOptions

data TagsOptions
    = OneTag TagId
    | TagsIn [TagId]
    | TagsAll [TagId]
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable TagsOptions

newtype SearchOptions =
    SearchOptions Text.Text
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable SearchOptions

data SortEntity
    = SEDate
    | SEAuthor
    | SECategory
    | SEPhotoNumber
  deriving (Show, Eq)

instance GP.PrettyShow SortEntity where
    prettyShow = GP.layoutStr . drop 2 . show

data SortOrder
    = SOAscending
    | SODescending
  deriving (Show, Eq)

instance GP.PrettyShow SortOrder where
    prettyShow = GP.layoutStr . drop 2 . show

data SortOptions =
    SortOptions
        { _so_sortBy :: SortEntity
        , _so_order :: SortOrder
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (GP.PrettyShow)


{-# LANGUAGE DeriveAnyClass #-}

module Types.Posts where

import Prelude hiding (readList)
import Types.Common

import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Void
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Types.Category
import Types.Tags

type PostId = Int

type CommentId = Int

type ActionComments
    = CRUD CreateComment (Paginated GetComments) Void DeleteComment

data GetComments =
   GetComments
      { _gc_postId :: PostId
      }
   deriving (Show, Eq, Generic, GP.PrettyShow)

data CreateComment =
   CreateComment
      { _ccom_postId :: PostId
      , _ccom_content :: T.Text
      }
   deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data DeleteComment =
   DeleteComment
      { _dc_commentId :: CommentId
      }
   deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data PublishEditPost =
   PublishEditPost
      { _pep_postId :: PostId
      , _pep_title :: T.Text
      , _pep_categoryId :: CategoryId
      , _pep_content :: T.Text
      , _pep_mainPhoto :: Maybe T.Text
      , _pep_extraPhotos :: Maybe [T.Text]
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

data CreationDateOptions
   = Created Time.Day
   | CreatedEarlier Time.Day
   | CreatedLater Time.Day
   deriving (Show, Eq, Generic)

data TagsOptions
   = OneTag TagId
   | TagsIn [TagId]
   | TagsAll [TagId]
   deriving (Show, Eq, Generic)

data SearchOptions =
   SearchOptions T.Text
   deriving (Show, Eq, Generic)

data SortEntity
   = SEDate
   | SEAuthor
   | SECategory
   | SEPhotoNumber
   deriving (Show, Eq)

data SortOrder
   = SOAscending
   | SODescending
   deriving (Show, Eq)

data SortOptions =
   SortOptions
      { _so_sortBy :: SortEntity
      , _so_order :: SortOrder
      }
   deriving (Show, Eq, Generic)

instance GP.PrettyShow CreationDateOptions where
   prettyShow = GP.LStr . show

instance GP.PrettyShow TagsOptions where
   prettyShow = GP.LStr . show

instance GP.PrettyShow SearchOptions where
   prettyShow = GP.LStr . show

instance GP.PrettyShow GetPosts

instance GP.PrettyShow SortEntity where
   prettyShow = GP.LStr . drop 2 . show

instance GP.PrettyShow SortOrder where
   prettyShow = GP.LStr . drop 2 . show

instance GP.PrettyShow SortOptions

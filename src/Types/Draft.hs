{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Draft
    ( DraftId
    , EditDraft(..)
    , EditDraftPublish(..)
    , GetDrafts(..)
    , Publish(..)
    , DeleteDraft(..)
    , CreateDraft(..)
    , ActionDrafts
    ) where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Prelude hiding (readList)
import Types.Category (CategoryId)
import Types.Common (CRUD, Paginated)
import Types.Posts (PostId)
import Types.Tags (TagId)

type DraftId = Int

type ActionDrafts
     = CRUD CreateDraft (Paginated GetDrafts) EditDraft DeleteDraft

data CreateDraft =
    CreateDraft
        { cdTitle :: Text.Text
        , cdTags :: [TagId]
        , cdCategoryId :: CategoryId
        , cdContent :: Text.Text
        , cdMainPhoto :: Maybe Text.Text
        , cdExtraPhotos :: Maybe [Text.Text]
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

data GetDrafts =
    GetDrafts
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable GetDrafts

data EditDraft =
    EditDraft
        { edDraftId :: DraftId
        , edTitle :: Maybe Text.Text
        , edTags :: Maybe [TagId]
        , edCategoryId :: Maybe CategoryId
        , edContent :: Maybe Text.Text
        , edMainPhoto :: Maybe Text.Text
        , edExtraPhotos :: Maybe [Text.Text]
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

data EditDraftPublish =
    EditDraftPublish
        { edpPostId :: PostId
        , edpDraftId :: DraftId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

newtype DeleteDraft =
    DeleteDraft
        { ddDraftId :: DraftId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

newtype Publish =
    Publish
        { pDraftId :: DraftId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)


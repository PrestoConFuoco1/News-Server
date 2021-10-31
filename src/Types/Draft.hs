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
        { _cd_title :: Text.Text
        , _cd_tags :: [TagId]
        , _cd_categoryId :: CategoryId
        , _cd_content :: Text.Text
        , _cd_mainPhoto :: Maybe Text.Text
        , _cd_extraPhotos :: Maybe [Text.Text]
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

data GetDrafts =
    GetDrafts
  deriving (Show, Eq, Generic)
  deriving GP.PrettyShow via GP.Showable GetDrafts

data EditDraft =
    EditDraft
        { _ed_draftId :: DraftId
        , _ed_title :: Maybe Text.Text
        , _ed_tags :: Maybe [TagId]
        , _ed_categoryId :: Maybe CategoryId
        , _ed_content :: Maybe Text.Text
        , _ed_mainPhoto :: Maybe Text.Text
        , _ed_extraPhotos :: Maybe [Text.Text]
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

data EditDraftPublish =
    EditDraftPublish
        { _edp_postId :: PostId
        , _edp_draftId :: DraftId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

newtype DeleteDraft =
    DeleteDraft
        { _dd_draft_id :: DraftId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

newtype Publish =
    Publish
        { _p_draftId :: DraftId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)


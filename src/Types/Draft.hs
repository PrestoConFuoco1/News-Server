{-# LANGUAGE DeriveAnyClass #-}

module Types.Draft where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Prelude hiding (readList)
import Types.Category
import Types.Common
import Types.Posts
import Types.Tags

type DraftId = Int

type ActionDrafts
    = CRUD CreateDraft (Paginated GetDrafts) EditDraft DeleteDraft

data CreateDraft =
   CreateDraft
      { _cd_title :: T.Text
    --_p_creationDate :: Time.Day,
    --_p_author :: Author,
      , _cd_tags :: [TagId]
      , _cd_categoryId :: CategoryId
      , _cd_content :: T.Text
      , _cd_mainPhoto :: Maybe T.Text
      , _cd_extraPhotos :: Maybe [T.Text]
      }
   deriving (Show, Eq, Generic, GP.PrettyShow)

data GetDrafts =
   GetDrafts
   deriving (Show, Eq, Generic)

instance GP.PrettyShow GetDrafts where
   prettyShow s = GP.LStr $ show s

data EditDraft =
   EditDraft
      { _ed_draftId :: DraftId
      , _ed_title :: Maybe T.Text
      , _ed_tags :: Maybe [TagId] --[TagId],
      , _ed_categoryId :: Maybe CategoryId --CategoryId,
      , _ed_content :: Maybe T.Text
      , _ed_mainPhoto :: Maybe T.Text
      , _ed_extraPhotos :: Maybe [T.Text]
      }
   deriving (Show, Eq, Generic, GP.PrettyShow)

data EditDraftPublish =
   EditDraftPublish
      { _edp_postId :: PostId
      , _edp_draftId :: DraftId
      }
   deriving (Show, Eq, Generic, GP.PrettyShow)

data DeleteDraft =
   DeleteDraft
      { _dd_draft_id :: DraftId
      }
   deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data Publish =
   Publish
      { _p_draftId :: DraftId
      }
   deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

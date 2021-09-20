{-# LANGUAGE DeriveAnyClass #-}
module Action.Draft.Types where


import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import Action.Tags.Types
import qualified Data.Time as Time
import Action.Common
import Data.Void





type ActionDrafts = CRUD CreateDraft GetDrafts EditDraft DeleteDraft


data CreateDraft = CreateDraft {
    _cd_title :: T.Text,
    --_p_creationDate :: Time.Day,
    --_p_author :: Author,
    _cd_tags :: [Int], --[TagId],
    _cd_categoryId :: Int, --CategoryId,
    _cd_content :: T.Text,
    _cd_mainPhoto :: Maybe T.Text,
    _cd_extraPhotos :: Maybe [T.Text]
 
    } deriving (Show, Generic, GP.PrettyShow)

data GetDrafts = GetDrafts
    deriving (Show, Generic)

instance GP.PrettyShow GetDrafts where
    prettyShow s = GP.LStr $ show s

data EditDraft = EditDraft {
    _ed_draftId :: Int,
    _ed_title :: Maybe T.Text,
    _ed_tags :: Maybe [Int], --[TagId],
    _ed_categoryId :: Maybe Int, --CategoryId,
    _ed_content :: Maybe T.Text,
    _ed_mainPhoto :: Maybe T.Text,
    _ed_extraPhotos :: Maybe [T.Text] 
    } deriving (Show, Generic, GP.PrettyShow)



data DeleteDraft = DeleteDraft {
    _dd_draft_id :: Int
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)


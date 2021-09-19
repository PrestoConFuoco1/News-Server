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





type ActionDrafts = CRUD CreateDraft Void Void Void


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

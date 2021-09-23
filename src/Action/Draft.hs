{-# LANGUAGE DeriveAnyClass #-}
module Action.Draft where

import Action.Utils
import Prelude hiding (readList)
import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import Action.Tags
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

data EditDraftPublish = EditDraftPublish {
    _edp_postId :: Int,
    _edp_draftId :: Int
    } deriving (Show, Generic, GP.PrettyShow)

data DeleteDraft = DeleteDraft {
    _dd_draft_id :: Int
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)


data Publish = Publish {
    _p_draftId :: Int
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)


createDraftToAction :: Router CreateDraft
createDraftToAction = do
    title <- requireField validateNotEmpty "title"
    tags <- requireField readList "tags" -- допустим ли пустой список?
    category <- requireField readInt "category_id"
    content <- requireField validateNotEmpty "content"
    mainPhoto <- optional validateNotEmpty  "main_photo"
    extraPhotos <- optional readList "extra_photos"
    return $ CreateDraft title tags category content mainPhoto extraPhotos

editDraftToAction :: Router EditDraft
editDraftToAction = do
    draftId <- requireField readInt "draft_id"
    title <- optional validateNotEmpty "title"
    tags <- optional readList "tags"
    category <- optional readInt "category_id"
    content <- optional validateNotEmpty "content"
    mainPhoto <- optional validateNotEmpty "main_photo"
    extraPhotos <- optional readList "extra_photos"
    return $ EditDraft draftId title tags category content mainPhoto extraPhotos
        

deleteDraftToAction :: Router DeleteDraft
deleteDraftToAction = do
    draftId <- requireField readInt "draft_id"
    return $ DeleteDraft draftId

publishAction :: Router Publish
publishAction = do
    draftId <- requireField readInt "draft_id"
    return $ Publish draftId
    

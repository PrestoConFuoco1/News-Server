{-# LANGUAGE DeriveAnyClass #-}
module Action.Tags where

import qualified Data.Text as T
import Action.Utils

import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import Action.Common

type TagId = Int


type ActionTags = CRUD CreateTag (Paginated GetTags) EditTag DeleteTag


data GetTags = GetTags
    deriving (Show, Generic)


data CreateTag = CreateTag {
    _ct_tagName :: T.Text
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data EditTag = EditTag {
    _et_tagId :: TagId,
    _et_tagName :: T.Text
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data DeleteTag = DeleteTag {
    _dt_tagId :: TagId
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

instance GP.PrettyShow GetTags where
    prettyShow = GP.LStr . show





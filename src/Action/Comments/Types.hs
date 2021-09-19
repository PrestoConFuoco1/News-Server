{-# LANGUAGE DeriveAnyClass #-}
module Action.Comments.Types where



import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
import Data.Void

type ActionComments = CRUD CreateComment GetComments Void Void -- DeleteComment

data GetComments = GetComments {
    _gc_postId :: Int
    } deriving (Show, Generic, GP.PrettyShow)

data CreateComment = CreateComment {
    _ccom_postId :: Int,
    _ccom_content :: T.Text
    -- user id will be known from auth token
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

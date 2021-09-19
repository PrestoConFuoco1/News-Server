{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts
#-}
module Delete where


import qualified Network.HTTP.Types as NHT
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception
import qualified Data.Aeson as Ae (Value, encode)

import qualified GenericPretty as GP
import GHC.Generics

import Action.RequestToAction
import Action.Types
import Action.Common

import qualified Logger as L
import MonadTypes
import qualified Database.PostgreSQL.Simple as PS
import qualified DatabaseHandler as DB
import qualified DBTypes as DBT
import qualified Types as Ty
import qualified Data.Aeson as Ae
import Data.Proxy
import qualified Control.Monad.Catch as CMC
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)

import Action.Tags.Types
import Action.Authors.Types
import Action.Category.Types
import Action.Users.Types

class (PS.ToRow (Del s)) => DeleteSQL s where
    type Del s :: *
    deleteQuery :: s -> PS.Query
    dName :: s -> B.ByteString

newtype DTag = DTag ()
dummyDTag = DTag ()

instance DeleteSQL DTag where
    type Del DTag = DeleteTag
    deleteQuery _ = "DELETE FROM news.tag WHERE tag_id = ?"
    dName _ = "tag"


newtype DCat = DCat ()
dummyDCat = DCat ()

instance DeleteSQL DCat where
    type Del DCat = DeleteCategory
    deleteQuery _ = "DELETE FROM news.category WHERE category_id = ?"
    dName _ = "category"


newtype DAuthor = DAuthor ()
dummyDAuthor = DAuthor ()

instance DeleteSQL DAuthor where
    type Del DAuthor = DeleteAuthor
    deleteQuery _ = "DELETE FROM news.author WHERE author_id = ?"
    dName _ = "author"

newtype DUser = DUser ()
dummyDUser = DUser ()

instance DeleteSQL DUser where
    type Del DUser = DeleteUser
    deleteQuery _ = "DELETE FROM news.users WHERE user_id = ?"
    dName _ = "user"




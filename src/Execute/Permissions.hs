{-# LANGUAGE RecordWildCards #-}
module Execute.Permissions where


import Execute.Actions
import qualified Network.HTTP.Types as NHT
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception
import Control.Monad (when)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified GenericPretty as GP
import GHC.Generics

import Action.RequestToAction
import Action.Types (WhoWhat (..), Token)
import Action.Common
import Database.Read
import Database.Create
import Database.Delete
import Database.Update

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Types as Ty
import qualified Data.Aeson as Ae
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import ActWithOne (actWithOne, ActWithOne(..), AWOu(..), AWOd(..))
import Execute.Types
import Execute.Utils
import Action.Users.Types
import Action.Comments.Types
import Action.Draft.Types
import Action.Authors.Types

import Exceptions as Ex

import Database.SqlValue
import Database.SqlQueryTypes
import Profiling


withAuthAdmin y = withAuth y >>= withAdmin

userAuthor :: (MonadServer m) => Ty.User -> m Ty.Author
userAuthor u = do
    as <- getThis' authorDummy (GetAuthors $ Just $ Ty._u_id u)
    a  <- validateUnique Ex.notAnAuthor as
    return a



withAuth :: (MonadServer m) => Maybe Token -> m (Maybe Ty.User)
withAuth mtoken = case mtoken of
    Nothing -> return Nothing
    Just token -> do
        users <- getUsersByToken token
        case users of 
            [] -> return Nothing
            [u] -> return $ Just u
            us  -> Ex.invalidUnique us

withAdmin :: (MonadServer m) => Maybe Ty.User -> m ()
withAdmin muser =
    when ((muser >>= Ty._u_admin) /= Just True) $ Ex.invalidEndpoint



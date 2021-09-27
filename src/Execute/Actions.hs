{-# LANGUAGE RecordWildCards #-}
module Execute.Actions where

import Prelude hiding (Read)
import qualified Data.Text as T (pack, Text)
import Control.Exception (displayException)
import Control.Monad (when)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified GenericPretty as GP

import Database.Read
import Database.Create
import Database.Delete
import Database.Update

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Data.Aeson as Ae
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch, throwM)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import Execute.Types
import Execute.Utils
import Execute.Database
import Result

import Exceptions as Ex

import Database.SqlValue
import Profiling (withTimePrint)
import Types

getUser :: (Monad m) => Maybe User -> m Response
getUser Nothing = Ex.unauthorized
getUser (Just u) = let val = Ae.toJSON u
                   in  return $ ok "Success" val


authenticate :: (MonadServer m) => Authenticate -> m Response
authenticate auth = do
    user <- getUserByLogin $ _au_login auth
    when (_u_passHash user /= _au_passHash auth) $
        CMC.throwM Ex.InvalidPassword
    token <- fmap (T.pack) $ randomString 10
    token' <- addToken (_u_id user) token
    return $ ok "Success" $ Ae.toJSON token'

addToken :: (MonadServer m) => UserId -> T.Text -> m T.Text
addToken id token = do
    let str = "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
        token' = (T.pack $ show id) <> token
    execute str (id, token', token')
    return token'


getUserByLogin :: (MonadServer m) => T.Text -> m User
getUserByLogin login = do
    let str = "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.users WHERE login = ?"
    users <- query str [login]
    validateUnique Ex.invalidLogin users




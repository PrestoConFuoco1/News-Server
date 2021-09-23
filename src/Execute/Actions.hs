{-# LANGUAGE RecordWildCards #-}
module Execute.Actions where

import Prelude hiding (Read)
import qualified Data.Text as T (pack, Text)
import Control.Exception (displayException)
import Control.Monad (when)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified GenericPretty as GP

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
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch, throwM)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
--import ActWithOne (actWithOne, ActWithOne(..), AWOu(..), AWOd(..))
import Execute.Types
import Execute.Utils
import Action.Users
import Action.Draft
import Execute.Result

import Exceptions as Ex

import Database.SqlValue
import Profiling (withTimePrint)


getUser :: (MonadServer m) => Maybe Ty.User -> m Response
getUser Nothing = Ex.unauthorized
getUser (Just u) = let val = Ae.toJSON u
                   in  return $ ok "Success" val


authenticate :: (MonadServer m) => Authenticate -> m Response
authenticate auth = do
    user <- getUserByLogin $ _au_login auth
    when (Ty._u_passHash user /= _au_passHash auth) $
        CMC.throwM Ex.InvalidPassword
    token <- fmap (T.pack) $ randomString 10
    token' <- addToken (Ty._u_id user) token
    return $ ok "Success" $ Ae.toJSON token'

addToken :: (MonadServer m) => Ty.UserId -> T.Text -> m T.Text
addToken id token = do
    let str = "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
        token' = (T.pack $ show id) <> token
    execute str (id, token', token')
    return token'


getUserByLogin :: (MonadServer m) => T.Text -> m Ty.User
getUserByLogin login = do
    let str = "SELECT u.user_id, u.firstname, u.lastname, \
              \u.image, u.login, u.pass_hash, u.creation_date, u.is_admin \
              \FROM news.users u WHERE u.login = ?"
    users <- query str [login]
    validateUnique Ex.invalidLogin users



createThis :: (MonadServer m, CreateSQL s) => s -> Create s -> m Response
createThis w cres = withExceptionHandlers
      (CMC.Handler (creExceptionHandler w)
      : Ex.defaultHandlers "createThis") $ do
    int <- createThis' w cres
    return $ okCreated (cName w <> " successfully created. " <> idInResult) int


getThis :: (Read s, MonadServer m) => s -> Get s -> m Response
getThis x g = do
--    cat <- f x g
    cat <- getThis' x g
    --logDebug $ T.pack $ GP.defaultPretty cat -- слишком много уже выдаётся
    let val = Ae.toJSON cat
    return $ ok "Success" val


deleteThis :: (MonadServer m, DeleteSQL s) => s -> Del s -> m Response
deleteThis s del =
    withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
        dels <- deleteThis' s del
        deleted <- validateUnique (Ex.throwDelNotFound $ dName s) dels
        let succ = dName s <> " successfully deleted"
        return $ okDeleted succ deleted


editThis :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Response
editThis s u = 
    withExceptionHandlers
      (CMC.Handler (updExceptionHandler s) :
      Ex.defaultHandlers "editThis") $ do
        id <- editThis' s u
        --actWithOne (AWOu s) num
        let succ = uName s <> " successfully edited"
        return (ok succ $ Ae.toJSON id)

updExceptionHandler :: (MonadServer m, UpdateSQL s) => s -> PS.SqlError -> m Response
updExceptionHandler s e = Ex.creUpdExceptionHandler (uName s) ("not implemented") ("not implemented") e

creExceptionHandler :: (MonadServer m, CreateSQL s) => s -> PS.SqlError -> m Response
creExceptionHandler s e = Ex.creUpdExceptionHandler (cName s) (cUniqueField s) (cForeign s) e


{-# LANGUAGE RecordWildCards #-}
module Execute.Actions where

import Prelude hiding (Read)
import qualified Data.Text as T (pack, Text)
import Control.Exception

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
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import ActWithOne (actWithOne, ActWithOne(..), AWOu(..), AWOd(..))
import Execute.Types
import Execute.Utils
import Action.Users.Types
import Action.Draft.Types

import Exceptions as Ex

import Database.SqlValue
import Profiling (withTimePrint)





getUser :: (MonadServer m) => Maybe Ty.User -> m Response
getUser Nothing = Ex.unauthorized
getUser (Just u) = let val = Ae.toJSON u
                   in  return $ ok val

validateUnique :: (MonadServer m, GP.PrettyShow a) => m a -> [a] -> m a
validateUnique x [] = x
validateUnique _ [a] = return a
validateUnique _ us  = Ex.invalidUnique us


maybeUserToUser :: (MonadServer m) => Maybe Ty.User -> m Ty.User
maybeUserToUser Nothing = Ex.unauthorized
maybeUserToUser (Just u) = return u

getUsersByToken :: (MonadServer m) => Token -> m [Ty.User]
getUsersByToken token = do
    let str = "SELECT u.user_id, u.firstname, u.lastname, \
              \u.image, u.login, u.pass_hash, u.creation_date, u.is_admin \
              \FROM news.token t JOIN news.users u ON t.user_id = u.user_id WHERE t.token = ?"
    users <- query str [token]
    return users


authenticate :: (MonadServer m) => Authenticate -> m Response
authenticate auth = do
    user <- getUserByLogin $ _au_login auth
    token <- fmap (T.pack) $ randomString 10
    token' <- addToken (Ty._u_id user) token
    return $ ok $ Ae.toJSON token'

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

idInResult = "id is int \"result\" field"

createThis :: (MonadServer m, CreateSQL s) => s -> Create s -> m Response
createThis w cres = do
    let str = createQuery w
    debugStr <- formatQuery str cres
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers [CMC.Handler (sqlH w)] $ do
        ints <- fmap (map PSTy.fromOnly) $ query str cres
        int <- validateUnique undefined ints
        return $ okCreated (cName w <> " successfully created. " <> idInResult) int
 where sqlH :: (MonadServer m, CreateSQL s) => s -> PS.SqlError -> m Response
       sqlH s e
            | uniqueConstraintViolated e = do
                logError $
                    "Failed to create new " <> cName s <> ", " <>
                    cUniqueField s <> " is already in use\n" <>
                    "SqlError: " <> (E.decodeUtf8 $ PS.sqlErrorMsg e)
                return $ bad $ cName s <> " with such " <> cUniqueField s <> " already exists."
            | foreignKeyViolated e = do
                let errmsg = 
                     "Failed to create new " <> cName s <> ", " <> cForeign s <> " is invalid"
                logError $ errmsg
                return $ bad $ errmsg
            | otherwise = logError (T.pack $ displayException e)
                >> return (internal "Internal error")



getThis' :: (Read s, MonadServer m) => s -> Get s -> m [MType s]
getThis' x g = do
        let (qu, pars) = selectQuery x g
        debugStr <- formatQuery qu pars
        logDebug $ T.pack $ show debugStr
        withTimePrint $ query qu pars




-- добавить обработку исключений!!!
getThis :: (Read s, MonadServer m) => s -> Get s -> m Response
getThis x g = do
--    cat <- f x g
    cat <- getThis' x g
    logDebug $ T.pack $ GP.defaultPretty cat
    let val = Ae.toJSON cat
    --return $ Response NHT.ok200 val
    return $ ok val



deleteThis :: (MonadServer m, DeleteSQL s) => s -> Del s -> m Response
deleteThis s d = do
    let (str, params) = deleteQuery s d
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
        num <- execute str params
        actWithOne (AWOd s) num
        let succ = dName s <> " successfully deleted"
        return (ok $ Ae.toJSON $ E.decodeUtf8 succ)
        
        



editThis' :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Int
editThis' s u = case updateParams s u of
  Nothing -> Ex.invalidUpdDel "No data to edit found, required at least one parameter"
  Just (q, vals) -> do
    let str = updateQuery s q
        params = vals ++ identifParams s u
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers (Ex.defaultHandlers "editThis") $ do
        num <- execute str params
        return num
 --       actWithOne (AWOu s) num

editThis :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Response
editThis s u = do
    num <- editThis' s u
    actWithOne (AWOu s) num
    let succ = uName s <> " successfully edited"
    return (ok $ Ae.toJSON $ E.decodeUtf8 succ)


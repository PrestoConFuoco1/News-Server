{-# LANGUAGE RecordWildCards #-}
module Execute.Actions where

import Prelude hiding (Read)
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



createDraft :: (MonadServer m) => WithAuthor CreateDraft -> m Response -- ?
createDraft (WithAuthor a CreateDraft{..}) = do
    let str =
         "INSERT INTO news.draft (title, author_id, category_id, content, photo, extra_photos) \
         \VALUES (?, ?, ?, ?, ?, ?) RETURNING draft_id"
        args = [SqlValue _cd_title, SqlValue a,
                SqlValue _cd_categoryId, SqlValue _cd_content,
                SqlValue _cd_mainPhoto, SqlValue $ fmap PSTy.PGArray _cd_extraPhotos]
    
    debugStr <- formatQuery str args
    logDebug $ T.pack $ show debugStr

--    id <- query str args >>= fmap (map PSTy.fromOnly) >>= \is -> validateUnique (Ex.invalidUnique is) $ is
    ids <- fmap (map PSTy.fromOnly) $ query str args
    id <- validateUnique undefined ids
    logDebug $ "Created draft with id = " <> (T.pack $ show id)
    tags <- attachTagsToDraft id _cd_tags
    return (ok $ "Draft successfully created")

attachTagsToDraft :: (MonadServer m) => Int -> [Int] -> m [Int]
attachTagsToDraft draftId tagsIds = do
    let str =
         "INSERT INTO news.draft_tag (draft_id, tag_id) VALUES "
        returning = " RETURNING tag_id"
        count = length tagsIds
        insertUnit = " ( ?, ? ) "
        insertUnits = maybe "" id $ intercalateQ $ replicate count insertUnit
        insertParams = map SqlValue $ foldr f [] tagsIds
        f x acc = draftId : x : acc
        qu = str <> insertUnits <> returning
    debugStr <- formatQuery qu insertParams
    logDebug $ T.pack $ show debugStr

    ids <- fmap (map PSTy.fromOnly) $ query qu insertParams
    logDebug $ "Attached tags with id in " <> (T.pack $ show ids)
    return ids

   


getUser :: (MonadServer m) => Maybe Ty.User -> m Response
getUser Nothing = Ex.unauthorized
getUser (Just u) = let val = Ae.toJSON u
                   in  return $ Response NHT.ok200 val

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


createThis :: (MonadServer m, CreateSQL s) => s -> Create s -> m Response
createThis w cres = do
    let str = createQuery w
    debugStr <- formatQuery str cres
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers [CMC.Handler (sqlH w)] $ do
        execute str cres
        return (ok $ Ae.toJSON $ E.decodeUtf8 $ cName w <> " successfully created")
  where sqlH :: (MonadServer m, CreateSQL s) => s -> PS.SqlError -> m Response
        sqlH s e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                    "Failed to create new " <> cName s <> ", " <>
                    cUniqueField s <> " is already in use\n" <>
                    "SqlError: " <> PS.sqlErrorMsg e
                return $ bad $ E.decodeUtf8 $ cName s <> " with such " <> cUniqueField s <> " already exists."
            | foreignKeyViolated e = do
                let errmsg = 
                     "Failed to create new " <> cName s <> ", " <> cForeign s <> " is invalid"
                logError $ E.decodeUtf8 $ errmsg
                return $ bad $ E.decodeUtf8 errmsg
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



editThis :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Response
editThis s u = case updateParams s u of
  Nothing -> undefined -- bad request
  Just (q, vals) -> do
    let str = updateQuery s q
        params = vals ++ identifParams s u
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers (Ex.defaultHandlers "editThis") $ do
        num <- execute str params
        actWithOne (AWOu s) num


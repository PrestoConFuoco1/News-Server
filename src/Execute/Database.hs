module Execute.Database where



import Prelude hiding (Read)
import qualified Data.Text as T (pack, Text)
import Control.Exception (displayException)
import Control.Monad (when)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified GenericPretty as GP

import Database

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal, MonadLog)
import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Data.Aeson as Ae
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch, throwM)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import Execute.Types
import Execute.Utils
import Result

import Exceptions as Ex

import Database.SqlValue
import Profiling (withTimePrint)
import Types
import Utils

{-
getThis :: (Read s, MonadServer m) => s -> Get s -> m Response
getThis x g = do
--    cat <- f x g
    cat <- getThis' x g
    --logDebug $ T.pack $ GP.defaultPretty cat -- слишком много уже выдаётся
    let val = Ae.toJSON cat
    return $ ok "Success" val
-}

getThis1 :: (Ae.ToJSON b, Monad m) => (a -> m [b]) -> a -> m Response
getThis1 f x = do
    cat <- f x
    let val = Ae.toJSON cat
    return $ ok "Success" val

{-
getThisPaginated :: (Read s, MonadServer m) => s -> Paginated (Get s) -> m Response
getThisPaginated x g = do
--    cat <- f x g
    cat <- getThisPaginated' x g
    --logDebug $ T.pack $ GP.defaultPretty cat -- слишком много уже выдаётся
    let val = Ae.toJSON cat
    return $ ok "Success" val
-}

createThis :: (MonadServer m, CreateSQL s) => s -> Create s -> m Response
createThis w cres = withExceptionHandlers
      (CMC.Handler (creExceptionHandler w)
      : Ex.defaultHandlers "createThis") $ do
    int <- createThis' w cres
    return $ okCreated (cName w <> " successfully created. " <> idInResult) int

createThis1 :: (CMC.MonadCatch m, MonadLog m) => T.Text -> (a -> m Int) -> a -> m Response
createThis1 name create x = withExceptionHandlers
      (CMC.Handler (Ex.creUpdExceptionHandler1 name)
      : Ex.defaultHandlers "createThis1") $ do
    int <- create x
    return $ okCreated (name <> " successfully created. " <> idInResult) int


deleteThis :: (MonadServer m, DeleteSQL s) => s -> Del s -> m Response
deleteThis s del =
    withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
        dels <- deleteThis' s del
        deleted <- validateUnique (Ex.throwDelNotFound $ dName s) dels
        let succ = dName s <> " successfully deleted"
        return $ okDeleted succ deleted

deleteThis1 :: (CMC.MonadCatch m, MonadLog m) => T.Text -> (a -> m [Int]) -> a -> m Response
deleteThis1 name delete x = withExceptionHandlers (Ex.defaultHandlers "deleteThis1") $ do
    dels <- delete x
    deleted <- validateUnique (Ex.throwDelNotFound name) dels
    let succ = name <> " successfully deleted"
    return $ okDeleted succ deleted

{-
editThis :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Response
editThis s u = 
    withExceptionHandlers
      (CMC.Handler (updExceptionHandler s) :
      Ex.defaultHandlers "editThis") $ do
        id <- editThis' s u
        --actWithOne (AWOu s) num
        let succ = uName s <> " successfully edited"
        return (ok succ $ Ae.toJSON id)
-}

editThis1 :: (CMC.MonadCatch m, MonadLog m) => T.Text -> (a -> m Int) -> a -> m Response
editThis1 name update x = withExceptionHandlers
      (CMC.Handler (Ex.creUpdExceptionHandler1 name)
      : Ex.defaultHandlers "editThis1") $ do
    id <- update x
    let succ = name <> " successfully edited"
    return (ok succ $ Ae.toJSON id)

updExceptionHandler :: (MonadServer m, UpdateSQL s) => s -> PS.SqlError -> m Response
updExceptionHandler s e = Ex.creUpdExceptionHandler (uName s) ("not implemented") ("not implemented") e

creExceptionHandler :: (MonadServer m, CreateSQL s) => s -> PS.SqlError -> m Response
creExceptionHandler s e = Ex.creUpdExceptionHandler (cName s) (cUniqueField s) (cForeign s) e



getThisPaginated' :: (Read s, MonadServer m) => s -> Paginated (Get s) -> m [MType s]
getThisPaginated' x (Paginated page size g) = do
    let (qu, pars) = selectQuery x g
        (qupag, parspag) = pageingClause page size
        qu' = qu <> qupag
        totalpars = pars ++ parspag
    debugStr <- formatQuery qu' totalpars
    logDebug $ T.pack $ show debugStr
    --withTimePrint $
    res <- query qu' totalpars
    logInfo $ "Fetched " <> showText (length res) <> " entities"
    return res

getThis' :: (Read s, MonadServer m) => s -> Get s -> m [MType s]
getThis' x g = do
    let (qu, pars) = selectQuery x g
    debugStr <- formatQuery qu pars
    logDebug $ T.pack $ show debugStr
    --withTimePrint $
    res <- query qu pars
    logInfo $ "Fetched " <> showText (length res) <> " entities"
    return res


editThis' :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Int
editThis' s u = case updateParams s u of
  Nothing -> Ex.invalidUpdDel "No data to edit found, required at least one parameter"
  Just (q, vals) -> do
    let str = updateQuery s q
        params = vals ++ identifParams s u
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    ids <- fmap (map PSTy.fromOnly) $ query str params
    id <- validateUnique (Ex.throwUpdNotFound $ uName s) ids
    logInfo $ "Updated " <> uName s <> " with id = " <> showText id
    return id


deleteThis' :: (MonadServer m, DeleteSQL s) => s -> Del s -> m [Int]
deleteThis' s del = do
    let (str, params) = deleteQuery s del
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    --withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
    ids <- fmap (map PSTy.fromOnly) $ query str params
    case ids of
        [] -> logInfo $ "No " <> dName s <> " deleted"
        _  -> logInfo $ "Deleted " <> dName s <> " with id = " <> showText ids
    return ids



createThis' :: (MonadServer m, CreateSQL s) => s -> Create s -> m Int
createThis' w cres = do
    let (str, params) = createQuery w cres
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    ints <- fmap (map PSTy.fromOnly) $ query str params
    int <- validateUnique (Ex.throwBadInsert (cName w)) ints
    logInfo $ "Created " <> cName w <> " with id = " <> showText int
    return int



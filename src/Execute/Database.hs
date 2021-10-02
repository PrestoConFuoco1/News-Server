module Execute.Database where


import Execute.Utils
import Prelude hiding (Read)
import qualified Data.Text as T (pack, Text)
import Control.Exception (displayException)
import Control.Monad (when)

import qualified GenericPretty as GP

import Database

import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Data.Aeson as Ae
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch, throwM)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import Execute.Types
import Result
import MonadLog
import MonadNews
import Exceptions as Ex

import Database.SqlValue
import Types

--getThis1 :: (CMC.MonadCatch m, Ae.ToJSON b)=> (a -> m [b]) -> a -> m APIResult
getThis1 :: (CMC.MonadCatch m, Gettable b) => (a -> m [b]) -> a -> m APIResult
getThis1 f x = do
    xs <- f x
 --   let val = Ae.toJSON cat
    --return $ ok "Success" val
    --return $ RGet $ RGettable cat
    return $ RGet $ RGettable xs

createThis1 :: (CMC.MonadCatch m, MonadLog m) => Entity -> (a -> m (Either ModifyError Int)) -> a -> m APIResult
createThis1 name create x = do
--      (CMC.Handler (Ex.creUpdExceptionHandler1 name)
--       : Ex.defaultHandlers "createThis1") $ do
    --int <- create x
    eithInt <- create x
    --return $ okCreated (name <> " successfully created. " <> idInResult) int
 --   return $ RCreated name int
    case eithInt of
        Right int -> return $ RCreated name int
        Left err -> return $ modifyErrorToApiResult name err

deleteErrorToApiResult :: Entity -> DeleteError -> APIResult
deleteErrorToApiResult ent DNoAction = RNotFound ent

deleteThis1 :: (CMC.MonadCatch m, MonadLog m) => Entity -> (a -> m (Either DeleteError Int)) -> a -> m APIResult
deleteThis1 name delete x = do --withExceptionHandlers (Ex.defaultHandlers "deleteThis1") $ do
    eithDeleted <- delete x
    --deleted <- validateUnique (hrowDelNotFound name) dels
    --let succ = name <> " successfully deleted"
    --return $ okDeleted succ deleted
    case eithDeleted of 
        Right int -> return $ RDeleted name int
        Left err -> return $ deleteErrorToApiResult name err

editThis1 :: (CMC.MonadCatch m, MonadLog m) => Entity -> (a -> m (Either ModifyError Int)) -> a -> m APIResult
editThis1 name update x = do
--      (CMC.Handler (Ex.creUpdExceptionHandler1 name)
--      : Ex.defaultHandlers "editThis1") $ do
    eithInt <- update x
    --let succ = name <> " successfully edited"
    --return (ok succ $ Ae.toJSON id)
    case eithInt of
        Right int -> return $ REdited name int
        Left err  -> return $ modifyErrorToApiResult name err


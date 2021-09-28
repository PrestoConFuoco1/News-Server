module Execute.Database where


import Execute.Utils
import Prelude hiding (Read)
import qualified Data.Text as T (pack, Text)
import Control.Exception (displayException)
import Control.Monad (when)

import qualified GenericPretty as GP

import Database

import MonadTypes (execute, query, formatQuery, ServerIO)
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


getThis1 :: (CMC.MonadCatch m, Ae.ToJSON b)=> (a -> m [b]) -> a -> m Response
getThis1 f x = do
    cat <- f x
    let val = Ae.toJSON cat
    return $ ok "Success" val

createThis1 :: (CMC.MonadCatch m, MonadLog m) => T.Text -> (a -> m Int) -> a -> m Response
createThis1 name create x = withExceptionHandlers
      (CMC.Handler (Ex.creUpdExceptionHandler1 name)
      : Ex.defaultHandlers "createThis1") $ do
    int <- create x
    return $ okCreated (name <> " successfully created. " <> idInResult) int

deleteThis1 :: (CMC.MonadCatch m, MonadLog m) => T.Text -> (a -> m [Int]) -> a -> m Response
deleteThis1 name delete x = withExceptionHandlers (Ex.defaultHandlers "deleteThis1") $ do
    dels <- delete x
    deleted <- validateUnique (Ex.throwDelNotFound name) dels
    let succ = name <> " successfully deleted"
    return $ okDeleted succ deleted

editThis1 :: (CMC.MonadCatch m, MonadLog m) => T.Text -> (a -> m Int) -> a -> m Response
editThis1 name update x = withExceptionHandlers
      (CMC.Handler (Ex.creUpdExceptionHandler1 name)
      : Ex.defaultHandlers "editThis1") $ do
    id <- update x
    let succ = name <> " successfully edited"
    return (ok succ $ Ae.toJSON id)


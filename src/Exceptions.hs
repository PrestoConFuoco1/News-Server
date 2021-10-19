module Exceptions where

import Control.Monad.Catch as CMC (Handler(..), catches, SomeException, throwM, Exception(..), MonadCatch(..), MonadThrow(..))
import Database.PostgreSQL.Simple as PS (sqlState, SqlError(..), QueryError, Query, ResultError, FormatError)
import Database.SqlValue
import qualified Database.PostgreSQL.Simple.Types as PSTy (fromQuery)
import qualified Data.Text as T (Text, pack)
import qualified GenericPretty as GP (PrettyShow(..), defaultPretty, textPretty)

import qualified Result as U
import Data.Text.Encoding as E (decodeUtf8)
import Types
import Utils (getPair, showText)
import qualified App.Logger as L


data ServerException =
    Default
    | SqlErrorAlreadyLogged
    | Unauthorized
    | InvalidUniqueEntities Entity [Int]
    | Forbidden
    | InvalidLogin
    | InvalidPassword
    | NotAnAuthor
    | InvalidUpdate
    | TokenShared [Int]
    deriving (Show, Eq)


throwForbidden :: (CMC.MonadThrow m) => m a
throwForbidden = CMC.throwM Forbidden

mainErrorHandler :: (MonadThrow m) => L.Handle m -> ServerException -> m U.Response
mainErrorHandler logger err = do
    L.logError logger $ T.pack $ displayException err
    mainErrorHandler' logger err

mainErrorHandler' :: (MonadThrow m) => L.Handle m -> ServerException -> m U.Response
mainErrorHandler' logger Default = return $ U.internal U.internalErrorMsg
mainErrorHandler' logger SqlErrorAlreadyLogged = return $ U.internal U.internalErrorMsg
mainErrorHandler' logger Unauthorized = return $ U.unauthorized U.unauthorizedMsg
mainErrorHandler' logger (InvalidUniqueEntities ent xs) = return $ U.internal U.internalErrorMsg
mainErrorHandler' logger Forbidden = return $ U.bad U.invalidEndpointMsg
mainErrorHandler' logger InvalidLogin = return $ U.bad $ U.invalidLoginMsg
mainErrorHandler' logger InvalidPassword = return $ U.bad U.invalidPasswordMsg
mainErrorHandler' logger NotAnAuthor = return $ U.bad U.notAnAuthorMsg
mainErrorHandler' logger (TokenShared xs) = --logError ("Token shared between users with id in " <> T.pack (show xs)) >>
        return (U.internal U.internalErrorMsg)

instance CMC.Exception ServerException


throwTokenShared :: (MonadThrow m) => [Int] -> m a
throwTokenShared lst = CMC.throwM $ TokenShared lst

throwInvalidUpdate :: (MonadThrow m) => m a
throwInvalidUpdate = CMC.throwM InvalidUpdate
throwUnauthorized :: (MonadThrow m) => m a
throwUnauthorized = CMC.throwM $ Unauthorized

throwInvalidUnique :: (MonadThrow m) => Entity -> [Int] -> m b
throwInvalidUnique ent xs = do
    --logError $ T.pack $ GP.defaultPretty xs
    CMC.throwM $ InvalidUniqueEntities ent xs


throwInvalidLogin :: (MonadThrow m)=> m a
throwInvalidLogin = CMC.throwM $ InvalidLogin

notAnAuthor :: (MonadThrow m) => m a
notAnAuthor = CMC.throwM $ NotAnAuthor


defaultSqlHandler :: (MonadThrow m) => L.Handle m -> PS.SqlError -> m a
defaultSqlHandler logger e = do
    L.logError logger "Unexpected SQL exception"
    L.logError logger $ (T.pack $ displayException e)
    CMC.throwM SqlErrorAlreadyLogged

formatErrorHandler :: (MonadThrow m) => L.Handle m -> PS.FormatError -> m a
formatErrorHandler logger e = do
    L.logError logger "SQL query format error"
    L.logError logger $ (T.pack $ displayException e)
    CMC.throwM SqlErrorAlreadyLogged

queryErrorHandler :: (MonadThrow m) => L.Handle m -> PS.QueryError -> m a
queryErrorHandler logger e = do
    L.logError logger $ "Query is used to perform an INSERT-like operation, \
             \or execute is used to perform a SELECT-like operation."
    L.logError logger (T.pack $ displayException e)
    CMC.throwM SqlErrorAlreadyLogged

resultErrorHandler :: (MonadThrow m) => L.Handle m -> PS.ResultError -> m a
resultErrorHandler logger e = do
    L.logError logger "Conversion from a SQL value to Haskell value failed."
    L.logError logger (T.pack $ displayException e)
    CMC.throwM SqlErrorAlreadyLogged

sqlHandlers :: (MonadThrow m, GP.PrettyShow q) => L.Handle m -> PS.Query -> q -> [Handler m a]
sqlHandlers h qu params = map f [
        Handler $ defaultSqlHandler h
        , Handler $ formatErrorHandler h
        , Handler $ queryErrorHandler h
        , Handler $ resultErrorHandler h
    ]
  where f (Handler func) = Handler $ \e -> do
            L.logError h $ "SQL query caused an exception"
            L.logError h $ "query is:"
            L.logError h $ E.decodeUtf8 $ PSTy.fromQuery qu
            L.logError h $ "params: "
            --L.logError h $ showText params
            L.logError h $ GP.textPretty params
            func e



withExceptionHandlers :: (Foldable f, CMC.MonadCatch m) => f (CMC.Handler m a) -> m a-> m a
withExceptionHandlers = flip CMC.catches

withHandler :: (CMC.MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
withHandler = flip CMC.catch

defaultMainHandler :: (MonadThrow m) => L.Handle m -> SomeException -> m U.Response
defaultMainHandler logger e = do
    L.logError logger $ T.pack $ displayException e
    return $ U.internal U.internalErrorMsg



uniqueConstraintViolated e = PS.sqlState e == "23505"
foreignKeyViolated e = PS.sqlState e == "23503"
constraintViolated e = PS.sqlState e == "23514"



--SqlError {sqlState = "23514", sqlExecStatus = FatalError,
--sqlErrorMsg = "new row for relation \"author\" violates check constraint
-- \"author_description_check\"", sqlErrorDetail =
-- "Failing row contains (26, 2, ).", sqlErrorHint = ""}

-- *** Exception: SqlError {sqlState = "23505", sqlExecStatus = FatalError, sqlErrorMsg =
--"duplicate key value violates unique constraint \"token_user_id_key\"",
--sqlErrorDetail = "Key (user_id)=(2) already exists.", sqlErrorHint = ""}

-- *** Exception: SqlError {sqlState = "23503", sqlExecStatus = FatalError, sqlErrorMsg =
--"insert or update on table \"token\" violates foreign key constraint \"token_user_id_fkey\"",
--sqlErrorDetail = "Key (user_id)=(6666) is not present in table \"users\".", sqlErrorHint = ""}


-- *** Exception: SqlError {sqlState = "23503", sqlExecStatus = FatalError, sqlErrorMsg =
-- "insert or update on table \"draft_tag\" violates foreign key constraint \"draft_tag_tag_id_fkey\"",
-- sqlErrorDetail = "Key (tag_id)=(167) is not present in table \"tag\".", sqlErrorHint = ""}


modifyErrorHandler :: (CMC.MonadCatch m) => PS.SqlError -> m ModifyError
modifyErrorHandler e = maybe (CMC.throwM e) return (toModifyError e)

toModifyError :: PS.SqlError -> Maybe ModifyError
toModifyError e
    | uniqueConstraintViolated e = fmap MAlreadyInUse $ getUniqueViolationData e
    | foreignKeyViolated e = fmap MInvalidForeign $ getForeignViolationData e
    | otherwise = Nothing


getUniqueViolationData :: PS.SqlError -> Maybe UniqueViolation
getUniqueViolationData = maybe Nothing (Just . f) . getPair . sqlErrorDetail
  where f (field, value) = UniqueViolation field value

getForeignViolationData :: PS.SqlError -> Maybe ForeignViolation
getForeignViolationData = maybe Nothing (Just . f) . getPair . sqlErrorDetail
  where f (field, value) = ForeignViolation field value




tagsErrorHandler :: (CMC.MonadCatch m) => PS.SqlError -> m TagsError
tagsErrorHandler e = maybe (CMC.throwM e) return (toAttachTagsError e)
  where toAttachTagsError e
            | foreignKeyViolated e = fmap TagsAttachError $ getForeignViolationData e
            | otherwise = Nothing



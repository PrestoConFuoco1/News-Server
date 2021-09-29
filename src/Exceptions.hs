module Exceptions where

import Control.Monad.Catch as CMC (Handler(..), catches, SomeException, throwM, Exception(..), MonadCatch(..), MonadThrow(..))
import MonadLog
import Database.PostgreSQL.Simple as PS (sqlState, SqlError(..), QueryError)
import qualified Data.Text as T (Text, pack)
import qualified GenericPretty as GP (PrettyShow(..), defaultPretty)

import qualified Result as U
import Data.Text.Encoding as E (decodeUtf8)
import Types
import Utils (getPair)


data ServerException =
      Default
    | Unauthorized
    | InvalidUniqueEntities Entity [Int]
    | Forbidden
    | InvalidLogin
    | InvalidPassword
    | NotAnAuthor
    | InvalidUpdate
    | TokenShared [Int]
    deriving (Show)


throwForbidden :: (CMC.MonadThrow m) => m a
throwForbidden = CMC.throwM Forbidden

{-
-}
mainErrorHandler :: (MonadThrow m, MonadLog m) => ServerException -> m U.Response
mainErrorHandler Default = return $ U.internal U.internalErrorMsg
mainErrorHandler Unauthorized = return $ U.unauthorized U.unauthorizedMsg
mainErrorHandler (InvalidUniqueEntities ent xs) = return $ U.internal U.internalErrorMsg
mainErrorHandler Forbidden = return $ U.bad U.invalidEndpointMsg
mainErrorHandler InvalidLogin = return $ U.bad $ U.invalidLoginMsg
mainErrorHandler InvalidPassword = return $ U.bad U.invalidPasswordMsg
mainErrorHandler NotAnAuthor = return $ U.bad U.notAnAuthorMsg
mainErrorHandler (TokenShared xs) = logError ("Token shared between users with id in " <> T.pack (show xs))
        >> return (U.internal U.internalErrorMsg)

instance CMC.Exception ServerException


throwTokenShared :: (MonadThrow m) => [Int] -> m a
throwTokenShared lst = CMC.throwM $ TokenShared lst

throwInvalidUpdate :: (MonadThrow m) => m a
throwInvalidUpdate = CMC.throwM InvalidUpdate
throwUnauthorized :: (MonadThrow m) => m a
throwUnauthorized = CMC.throwM $ Unauthorized
{-
-}
throwInvalidUnique :: (MonadThrow m) => Entity -> [Int] -> m b
throwInvalidUnique ent xs = do
--    logError $ T.pack $ GP.defaultPretty xs
    CMC.throwM $ InvalidUniqueEntities ent xs


throwInvalidLogin :: (MonadThrow m, MonadLog m) => m a
throwInvalidLogin = CMC.throwM $ InvalidLogin

notAnAuthor :: (MonadThrow m, MonadLog m) => m a
notAnAuthor = CMC.throwM $ NotAnAuthor

defaultSqlHandler :: (MonadThrow m, MonadLog m) => T.Text -> PS.SqlError -> m a
defaultSqlHandler funcMsg e = do
    logError "Some not caught exception"
    logError funcMsg
    logError (T.pack $ displayException e)
    CMC.throwM Default

queryErrorHandler :: (MonadThrow m, MonadLog m) => T.Text -> PS.QueryError -> m a
queryErrorHandler funcMsg e = do
    logError "Query is used to perform an INSERT-like operation, \
             \or execute is used to perform a SELECT-like operation."
    logError funcMsg
    logError (T.pack $ displayException e)
    CMC.throwM Default

resultErrorHandler :: (MonadThrow m, MonadLog m) => T.Text -> PS.QueryError -> m a
resultErrorHandler funcMsg e = do
    logError "Conversion from a SQL value to Haskell value failed."
    logError funcMsg
    logError (T.pack $ displayException e)
    CMC.throwM Default

defaultHandlers :: (MonadThrow m, MonadLog m) => T.Text -> [Handler m a]
defaultHandlers funcMsg = [Handler $ queryErrorHandler funcMsg,
                           Handler $ resultErrorHandler funcMsg,
                           Handler $ defaultSqlHandler funcMsg]




withExceptionHandlers :: (Foldable f, CMC.MonadCatch m) => f (CMC.Handler m a) -> m a-> m a
withExceptionHandlers = flip CMC.catches

withHandler :: (CMC.MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
withHandler = flip CMC.catch

defaultMainHandler :: (MonadLog m, MonadThrow m) => SomeException -> m U.Response
defaultMainHandler e = do
    logError $ T.pack $ displayException e
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



module Exceptions
    ( sqlHandlers
    , withHandler
    , withExceptionHandlers
    , throwInvalidUnique
    , tagsErrorHandler
    , throwTokenShared
    , throwInvalidUpdate
    , modifyErrorHandler
    , throwForbidden
    , throwUnauthorized
    , throwInvalidLogin
    , throwInvalidPassword
    , notAnAuthor
    , defaultMainHandler
    , mainErrorHandler
    , ServerException(..) -- only for tests
    ) where

import Control.Monad.Catch as CMC
    ( Exception(..)
    , Handler(..)
    , MonadCatch(..)
    , MonadThrow(..)
    , SomeException
    , catches
    , throwM
    )
import qualified Data.Text as T (Text, pack)
import Database.PostgreSQL.Simple as PS
    ( FormatError
    , Query
    , QueryError
    , ResultError
    , SqlError(..)
    , sqlState
    )
import qualified Database.PostgreSQL.Simple.Types as PSTy (fromQuery)
import qualified GenericPretty as GP (PrettyShow(..), textPretty)

import qualified App.Logger as L
import Data.Text.Encoding as E (decodeUtf8)
import qualified Result as U
import qualified Types as Y
import Utils (getPair)

data ServerException
    = Default
    | SqlErrorAlreadyLogged
    | Unauthorized
    | InvalidUniqueEntities Y.Entity [Int]
    | Forbidden
    | InvalidLogin
    | InvalidPassword
    | NotAnAuthor
    | InvalidUpdate T.Text
    | TokenShared [Int]
  deriving (Show, Eq)

throwForbidden :: (CMC.MonadThrow m) => m a
throwForbidden = CMC.throwM Forbidden

mainErrorHandler ::
       (MonadThrow m)
    => L.LoggerHandler m
    -> ServerException
    -> m U.Response
mainErrorHandler logger err = do
    L.logError logger $ T.pack $ displayException err
    mainErrorHandler' logger err

mainErrorHandler' ::
       (MonadThrow m)
    => L.LoggerHandler m
    -> ServerException
    -> m U.Response
mainErrorHandler' _ Default = pure $ U.internal U.internalErrorMsg
mainErrorHandler' _ SqlErrorAlreadyLogged =
    pure $ U.internal U.internalErrorMsg
mainErrorHandler' _ Unauthorized =
    pure $ U.unauthorized U.unauthorizedMsg
mainErrorHandler' _ (InvalidUniqueEntities _ _) =
    pure $ U.internal U.internalErrorMsg
mainErrorHandler' _ Forbidden = pure $ U.bad U.invalidEndpointMsg
mainErrorHandler' _ InvalidLogin = pure $ U.bad U.invalidLoginMsg
mainErrorHandler' _ (InvalidUpdate details) =
    pure $ U.bad $ "invalid data to update: " <> details
mainErrorHandler' _ InvalidPassword =
    pure $ U.bad U.invalidPasswordMsg
mainErrorHandler' _ NotAnAuthor = pure $ U.bad U.notAnAuthorMsg
mainErrorHandler' _ (TokenShared _) =
    pure (U.internal U.internalErrorMsg)

instance CMC.Exception ServerException

throwTokenShared :: (MonadThrow m) => [Int] -> m a
throwTokenShared lst = CMC.throwM $ TokenShared lst

throwInvalidUpdate :: (MonadThrow m) => T.Text -> m a
throwInvalidUpdate = CMC.throwM . InvalidUpdate

throwUnauthorized :: (MonadThrow m) => m a
throwUnauthorized = CMC.throwM Unauthorized

throwInvalidUnique :: (MonadThrow m) => Y.Entity -> [Int] -> m b
throwInvalidUnique ent xs = do
    CMC.throwM $ InvalidUniqueEntities ent xs

throwInvalidLogin :: (MonadThrow m) => m a
throwInvalidLogin = CMC.throwM InvalidLogin

throwInvalidPassword :: (MonadThrow m) => m a
throwInvalidPassword = CMC.throwM InvalidPassword

notAnAuthor :: (MonadThrow m) => m a
notAnAuthor = CMC.throwM NotAnAuthor

defaultSqlHandler ::
       (MonadThrow m) => L.LoggerHandler m -> PS.SqlError -> m a
defaultSqlHandler logger e = do
    L.logError logger "Unexpected SQL exception"
    L.logError logger $ T.pack $ displayException e
    CMC.throwM SqlErrorAlreadyLogged

formatErrorHandler ::
       (MonadThrow m) => L.LoggerHandler m -> PS.FormatError -> m a
formatErrorHandler logger e = do
    L.logError logger "SQL query format error"
    L.logError logger $ T.pack $ displayException e
    CMC.throwM SqlErrorAlreadyLogged

queryErrorHandler ::
       (MonadThrow m) => L.LoggerHandler m -> PS.QueryError -> m a
queryErrorHandler logger e = do
    L.logError
        logger
        "Query is used to perform an INSERT-like operation, \
             \or execute is used to perform a SELECT-like operation."
    L.logError logger (T.pack $ displayException e)
    CMC.throwM SqlErrorAlreadyLogged

resultErrorHandler ::
       (MonadThrow m) => L.LoggerHandler m -> PS.ResultError -> m a
resultErrorHandler logger e = do
    L.logError
        logger
        "Conversion from a SQL value to Haskell value failed."
    L.logError logger (T.pack $ displayException e)
    CMC.throwM SqlErrorAlreadyLogged

sqlHandlers ::
       (MonadThrow m, GP.PrettyShow q)
    => L.LoggerHandler m
    -> PS.Query
    -> q
    -> [Handler m a]
sqlHandlers h qu params =
    map f
        [ Handler $ defaultSqlHandler h
        , Handler $ formatErrorHandler h
        , Handler $ queryErrorHandler h
        , Handler $ resultErrorHandler h
        ]
  where
    f (Handler func) =
        Handler $ \e -> do
            L.logError h "SQL query caused an exception"
            L.logError h "query is:"
            L.logError h $ E.decodeUtf8 $ PSTy.fromQuery qu
            L.logError h "params: "
            L.logError h $ GP.textPretty params
            func e

withExceptionHandlers ::
       (Foldable f, CMC.MonadCatch m)
    => f (CMC.Handler m a)
    -> m a
    -> m a
withExceptionHandlers = flip CMC.catches

withHandler ::
       (CMC.MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
withHandler = flip CMC.catch

defaultMainHandler ::
       (MonadThrow m)
    => L.LoggerHandler m
    -> SomeException
    -> m U.Response
defaultMainHandler logger e = do
    L.logError logger $ T.pack $ displayException e
    pure $ U.internal U.internalErrorMsg

uniqueConstraintViolated, foreignKeyViolated, constraintViolated ::
       PS.SqlError -> Bool
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
modifyErrorHandler ::
       (CMC.MonadCatch m) => PS.SqlError -> m Y.ModifyError
modifyErrorHandler e = maybe (CMC.throwM e) pure (toModifyError e)

toModifyError :: PS.SqlError -> Maybe Y.ModifyError
toModifyError e
    | uniqueConstraintViolated e =
        Y.MAlreadyInUse <$> getUniqueViolationData e
    | foreignKeyViolated e =
        Y.MInvalidForeign <$> getForeignViolationData e
    | otherwise = Nothing

getUniqueViolationData :: PS.SqlError -> Maybe Y.UniqueViolation
getUniqueViolationData = fmap f . getPair . sqlErrorDetail
  where
    f (field, value) = Y.UniqueViolation field value

getForeignViolationData :: PS.SqlError -> Maybe Y.ForeignViolation
getForeignViolationData = fmap f . getPair . sqlErrorDetail
  where
    f (field, value) = Y.ForeignViolation field value

tagsErrorHandler :: (CMC.MonadCatch m) => PS.SqlError -> m Y.TagsError
tagsErrorHandler e = maybe (CMC.throwM e) pure (toAttachTagsError e)
  where
    toAttachTagsError err
        | foreignKeyViolated err =
            Y.TagsAttachError <$> getForeignViolationData err
        | otherwise = Nothing

module Execute.Database
    ( createThis
    , editThis
    , getThis
    , deleteThis
    ) where

import qualified Control.Monad.Catch as CMC (MonadCatch)
import qualified Types as T

getThis ::
       (CMC.MonadCatch m, T.Gettable b)
    => (a -> m [b])
    -> a
    -> m T.APIResult
getThis f x = T.RGet . T.RGettable <$> f x

createThis ::
       (CMC.MonadCatch m)
    => T.Entity
    -> (a -> m (Either T.ModifyError Int))
    -> a
    -> m T.APIResult
createThis name create x = do
    eithInt <- create x
    case eithInt of
        Right int -> pure $ T.RCreated name int
        Left err -> pure $ T.RFailed name err

deleteErrorToApiResult :: T.Entity -> T.DeleteError -> T.APIResult
deleteErrorToApiResult ent T.DeleteNoAction = T.RFailed ent T.MNoAction

deleteThis ::
       (CMC.MonadCatch m)
    => T.Entity
    -> (a -> m (Either T.DeleteError Int))
    -> a
    -> m T.APIResult
deleteThis name delete x = do
    eithDeleted <- delete x
    case eithDeleted of
        Right int -> pure $ T.RDeleted name int
        Left err -> pure $ deleteErrorToApiResult name err

editThis ::
       (CMC.MonadCatch m)
    => T.Entity
    -> (a -> m (Either T.ModifyError Int))
    -> a
    -> m T.APIResult
editThis name update x = do
    eithInt <- update x
    case eithInt of
        Right int -> pure $ T.REdited name int
        Left err -> pure $ T.RFailed name err

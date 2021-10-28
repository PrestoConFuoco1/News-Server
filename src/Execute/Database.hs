module Execute.Database where

import qualified Control.Monad.Catch as CMC (MonadCatch)
import Execute.Utils (modifyErrorToApiResult)
import qualified Types as Y

getThis ::
      (CMC.MonadCatch m, Y.Gettable b)
   => (a -> m [b])
   -> a
   -> m Y.APIResult
getThis f x = Y.RGet . Y.RGettable <$> f x

createThis ::
      (CMC.MonadCatch m)
   => Y.Entity
   -> (a -> m (Either Y.ModifyError Int))
   -> a
   -> m Y.APIResult
createThis name create x = do
   eithInt <- create x
   case eithInt of
      Right int -> pure $ Y.RCreated name int
      Left err -> pure $ modifyErrorToApiResult name err

deleteErrorToApiResult :: Y.Entity -> Y.DeleteError -> Y.APIResult
deleteErrorToApiResult ent Y.DNoAction = Y.RNotFound ent

deleteThis ::
      (CMC.MonadCatch m)
   => Y.Entity
   -> (a -> m (Either Y.DeleteError Int))
   -> a
   -> m Y.APIResult
deleteThis name delete x = do
   eithDeleted <- delete x
   case eithDeleted of
      Right int -> pure $ Y.RDeleted name int
      Left err -> pure $ deleteErrorToApiResult name err

editThis ::
      (CMC.MonadCatch m)
   => Y.Entity
   -> (a -> m (Either Y.ModifyError Int))
   -> a
   -> m Y.APIResult
editThis name update x = do
   eithInt <- update x
   case eithInt of
      Right int -> pure $ Y.REdited name int
      Left err -> pure $ modifyErrorToApiResult name err

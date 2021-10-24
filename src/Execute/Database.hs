module Execute.Database where

import qualified Control.Monad.Catch as CMC (MonadCatch)
import Execute.Utils (modifyErrorToApiResult)
import Types

getThis ::
      (CMC.MonadCatch m, Gettable b)
   => (a -> m [b])
   -> a
   -> m APIResult
getThis f x = (RGet . RGettable) <$> f x

createThis ::
      (CMC.MonadCatch m)
   => Entity
   -> (a -> m (Either ModifyError Int))
   -> a
   -> m APIResult
createThis name create x = do
   eithInt <- create x
   case eithInt of
      Right int -> pure $ RCreated name int
      Left err -> pure $ modifyErrorToApiResult name err

deleteErrorToApiResult :: Entity -> DeleteError -> APIResult
deleteErrorToApiResult ent DNoAction = RNotFound ent

deleteThis ::
      (CMC.MonadCatch m)
   => Entity
   -> (a -> m (Either DeleteError Int))
   -> a
   -> m APIResult
deleteThis name delete x = do
   eithDeleted <- delete x
   case eithDeleted of
      Right int -> pure $ RDeleted name int
      Left err -> pure $ deleteErrorToApiResult name err

editThis ::
      (CMC.MonadCatch m)
   => Entity
   -> (a -> m (Either ModifyError Int))
   -> a
   -> m APIResult
editThis name update x = do
   eithInt <- update x
   case eithInt of
      Right int -> pure $ REdited name int
      Left err -> pure $ modifyErrorToApiResult name err

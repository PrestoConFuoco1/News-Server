module Execute.Utils where

import qualified App.Database as D
import Control.Monad (when)
import qualified Control.Monad.Catch as CMC
import qualified Data.Text as T
import qualified Exceptions as Ex
import qualified GenericPretty as GP
import Types

withAuthAdmin ::
      (CMC.MonadThrow m)
   => D.Handle m
   -> Maybe Token
   -> m ()
withAuthAdmin h y = do
   muser <- withAuth h y
   checkAdmin h muser

withAuthor ::
      (CMC.MonadThrow m)
   => D.Handle m
   -> Maybe Token
   -> m Author
withAuthor h y = do
   muser <- withAuth h y
   user <- maybeUserToUser h muser
   mauthor <- D.userAuthor h (D.log h) user
   maybe Ex.notAnAuthor pure mauthor

withAuth ::
      (Monad m)
   => D.Handle m
   -> Maybe Token
   -> m (Maybe User)
withAuth h mtoken = do
   let fname = "withAuth: "
   D.logDebug h $ fname <> "trying to get user by token"
   case mtoken of
         Nothing -> do
            D.logDebug h $ fname <> "no token supplied"
            pure Nothing
         Just token -> do
            D.logDebug h $
               fname <>
               "searching for user with token = " <> _t_token token
            D.getUserByToken h (D.log h) token

checkAdmin ::
      (CMC.MonadThrow m) => D.Handle m -> Maybe User -> m ()
checkAdmin h muser = do
   let fname = "checkAdmin: "
   case muser of
      Nothing -> do
         D.logDebug h $
            fname <> "no user found, throwing forbidden"
         Ex.throwForbidden
      Just user -> do
         D.logDebug h $ fname <> "found user"
         D.logDebug h $ GP.textPretty user
         if _u_admin user /= Just True
            then do
               D.logDebug h $
                  fname <>
                  "user is not admin, throwing forbidden"
               Ex.throwForbidden
            else do
               D.logDebug h $ fname <> "ok, user is admin"
               pure ()

maybeUserToUser ::
      (CMC.MonadThrow m)
   => D.Handle m
   -> Maybe User
   -> m User
maybeUserToUser h Nothing = do
   let fname = "maybeUserToUser: "
   D.logDebug h $
      fname <> "no user found, throwing unauthorized"
   Ex.throwUnauthorized
maybeUserToUser h (Just u) = do
   let fname = "maybeUserToUser: "
   D.logDebug h $ fname <> "found user"
   D.logDebug h $ GP.textPretty u
   pure u

getUser ::
      (CMC.MonadThrow m)
   => D.Handle m
   -> Maybe User
   -> m APIResult
getUser h muser = do
   user <- maybeUserToUser h muser
   pure $ RGetUser user

getUserFname :: String
getUserFname = "getUser: "

authenticate ::
      (CMC.MonadThrow m)
   => D.Handle m
   -> Authenticate
   -> m APIResult
authenticate h auth = do
   muser <- D.getUserByLogin h (D.log h) $ _au_login auth
   user <- maybe Ex.throwInvalidLogin pure muser
   when (_u_passHash user /= _au_passHash auth) $
      CMC.throwM Ex.InvalidPassword
   token <- T.pack <$> D.generateToken h 10
   token' <- D.addToken h (D.log h) (_u_id user) token
   pure $ RGetToken token'

modifyErrorToApiResult :: Entity -> ModifyError -> APIResult
modifyErrorToApiResult ent (MAlreadyInUse (UniqueViolation field value)) =
   RAlreadyInUse ent field value
modifyErrorToApiResult ent (MInvalidForeign (ForeignViolation field value)) =
   RInvalidForeign ent field value
modifyErrorToApiResult ent MNoAction = RNotFound ent

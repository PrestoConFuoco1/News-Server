module Execute.Utils where

import qualified Control.Monad.Catch as CMC
import qualified Exceptions as Ex
import MonadNews
import Types
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Aeson as Ae
import Result

withAuthAdmin y = withAuth y >>= checkAdmin

withAuthor y = 
    withAuth y >>= maybeUserToUser >>= userAuthor


withAuth :: (MonadNews m) => Maybe Token -> m (Maybe User)
withAuth mtoken = case mtoken of
    Nothing -> return Nothing
    Just token -> do
        muser <- getUserByToken token
        return muser

checkAdmin :: (CMC.MonadThrow m) => Maybe User -> m ()
checkAdmin muser =
   when ((muser >>= _u_admin) /= Just True) $ Ex.throwForbidden


{-
-}
maybeUserToUser :: (CMC.MonadThrow m) => Maybe User -> m User
maybeUserToUser Nothing = Ex.unauthorized
maybeUserToUser (Just u) = return u

validateUnique :: (CMC.MonadThrow m) => m a -> [a] -> m a
validateUnique x [] = x
validateUnique _ [a] = return a
validateUnique _ us  = Ex.invalidUnique us


validateUnique2 :: (Monad m) => m a -> m a -> [a] -> m a
validateUnique2 empty toomuch [] = empty
validateUnique2 empty toomuch [a] = return a
validateUnique2 empty toomuch us = toomuch


getUser :: (CMC.MonadThrow m) => Maybe User -> m Response
getUser Nothing = Ex.unauthorized
getUser (Just u) = let val = Ae.toJSON u
                   in  return $ ok "Success" val


authenticate :: (MonadNews m) => Authenticate -> m Response
authenticate auth = do
    user <- getUserByLogin $ _au_login auth
    when (_u_passHash user /= _au_passHash auth) $
        CMC.throwM Ex.InvalidPassword
    token <- fmap (T.pack) $ randomString1 10
    token' <- addToken (_u_id user) token
    return $ ok "Success" $ Ae.toJSON token'




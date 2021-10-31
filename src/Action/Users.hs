module Action.Users
    ( createUserToAction
    , deleteUserToAction
    ) where

import Action.Common (Router)
import qualified Action.Utils as AU
import qualified Types as T

createUserToAction :: Router T.CreateUser
createUserToAction = do
    login <- AU.requireField AU.validateNotEmpty "login"
    passHash <- AU.requireField AU.validateNotEmpty "pass_hash"
    firstName <- AU.requireField AU.validateNotEmpty "firstname"
    lastName <- AU.requireField AU.validateNotEmpty "lastname"
    pure $ T.CreateUser login passHash firstName lastName

deleteUserToAction :: Router T.DeleteUser
deleteUserToAction = do
    uid <- AU.requireField AU.readInt "user_id"
    pure $ T.DeleteUser uid

module Action.Users where

import Action.Common (Router)
import qualified Action.Utils as AU
import Types

createUserToAction :: Router CreateUser
createUserToAction = do
   login <- AU.requireField AU.validateNotEmpty "login"
   passHash <- AU.requireField AU.validateNotEmpty "pass_hash"
   firstName <- AU.requireField AU.validateNotEmpty "firstname"
   lastName <- AU.requireField AU.validateNotEmpty "lastname"
   pure $ CreateUser login passHash firstName lastName

deleteUserToAction :: Router DeleteUser
deleteUserToAction = do
   uid <- AU.requireField AU.readInt "user_id"
   pure $ DeleteUser uid

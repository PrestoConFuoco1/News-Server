module Action.Users where

import Action.Common (Router)
import Action.Utils
import Types

createUserToAction :: Router CreateUser
createUserToAction = do
   login <- requireField validateNotEmpty "login"
   passHash <- requireField validateNotEmpty "pass_hash"
   firstName <- requireField validateNotEmpty "firstname"
   lastName <- requireField validateNotEmpty "lastname"
   pure $ CreateUser login passHash firstName lastName

deleteUserToAction :: Router DeleteUser
deleteUserToAction = do
   uid <- requireField readInt "user_id"
   pure $ DeleteUser uid

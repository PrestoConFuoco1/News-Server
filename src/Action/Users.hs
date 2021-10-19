module Action.Users where

import Action.Utils
import Action.Common
import Types

createUserToAction :: Router CreateUser
createUserToAction = do
    login <- requireField validateNotEmpty "login"
    passHash <- requireField validateNotEmpty "pass_hash"
    firstName <- requireField validateNotEmpty "firstname"
    lastName <- requireField validateNotEmpty "lastname"
    return $ CreateUser login passHash firstName lastName


deleteUserToAction :: Router DeleteUser
deleteUserToAction = do
    id <- requireField readInt "user_id"
    return $ DeleteUser id


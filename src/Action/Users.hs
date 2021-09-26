{-# LANGUAGE DeriveAnyClass #-}
module Action.Users where


import Action.Utils

import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import Data.Void
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


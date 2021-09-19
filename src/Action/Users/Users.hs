module Action.Users.Users where


import Action.Utils
import Action.Users.Types
import qualified Data.Text as T
import Action.Common



requestToActionUsers :: [T.Text] -> Query -> Either ActionError ActionUsers
requestToActionUsers path hash = case path of
  (x:[])
    | x == "profile" -> return $ Read GetProfile
    | x == "create" -> fmap Create $ createUserToAction hash
    | x == "delete" -> fmap Delete $ deleteUserToAction hash
  _ -> Left EInvalidEndpoint

createUserToAction :: Query -> Either ActionError CreateUser
createUserToAction hash = do
    login <- requireField (requireText hash) "login"
    passHash <- requireField (requireText hash) "pass_hash"
    firstName <- requireField (requireText hash) "firstname"
    lastName <- requireField (requireText hash) "lastname"
    return $ CreateUser login passHash firstName lastName


deleteUserToAction :: Query -> Either ActionError DeleteUser
deleteUserToAction hash = do
    id <- requireField (requireInt hash) "user_id"
    return $ DeleteUser id

requestToActionAuthenticate :: [T.Text] -> Query -> Either ActionError Authenticate
requestToActionAuthenticate xs hash = do
    login <- requireField (requireText hash) "login"
    passHash <- requireField (requireText hash) "pass_hash"
    return $ Authenticate login passHash

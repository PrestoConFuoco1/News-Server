module Action.Users.Users where


import Action.Utils
import Action.Users.Types
import qualified Data.Text as T
import Action.Common



requestToActionUsers :: [T.Text] -> Query -> Either ActionError ActionUsers
requestToActionUsers path hash = case path of
  (x:xs)
 --   | x == "get" -> AGetCategories GetCategories
    | x == "create" -> fmap Create $ createUserToAction hash
  [] -> Left EInvalidEndpoint

createUserToAction :: Query -> Either ActionError CreateUser
createUserToAction hash = do
    login <- requireField (requireText hash) "login"
    passHash <- requireField (requireText hash) "pass_hash"
    firstName <- requireField (requireText hash) "firstname"
    lastName <- requireField (requireText hash) "lastname"
    return $ CreateUser login passHash firstName lastName



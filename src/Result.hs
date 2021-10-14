{-# LAnGUAGE DeriveAnyClass #-}
module Result where


import qualified Network.HTTP.Types as NHT
import qualified Data.Aeson as Ae

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import GHC.Generics
import Types
import Utils

successGet = "Success" :: T.Text
successGetProfile = "Got profile successfully" :: T.Text
successNewToken = "Got token successfully" :: T.Text

forbidden = "Access only for administrators, sending 404 invalid endpoint." :: T.Text

badInsert = "Bad insertion" :: T.Text

idInResult = "id is int \"result\" field" :: T.Text

unauthorizedMsg = "Unauthorized, use /auth" :: T.Text
invalidPasswordMsg = "Invalid password" :: T.Text
invalidLoginMsg = "Invalid login" :: T.Text

invalidEndpointMsg = "Invalid endpoint" :: T.Text
internalErrorMsg = "Internal error" :: T.Text

notAnAuthorMsg = "Not an author" :: T.Text

createdMsg :: Entity -> T.Text
createdMsg ent = let enttext = showEText ent
    in "Successfully created " <> enttext <> ", " <> enttext <> "_id is in \"result\" field"


editedMsg :: Entity -> T.Text
editedMsg ent = let enttext = showEText ent
    in "Successfully edited " <> enttext <> ", " <> enttext <> "_id is in \"result\" field"

deletedMsg :: Entity -> T.Text
deletedMsg ent = let enttext = showEText ent
    in "Successfully deleted " <> enttext <> ", " <> enttext <> "_id is in \"result\" field"

entityNotFoundMsg :: Entity -> T.Text
entityNotFoundMsg ent = let enttext = showEText ent
                     in  enttext <> " not found"

alreadyInUseMsg :: Entity -> T.Text -> T.Text -> T.Text
alreadyInUseMsg ent field value =
    "Failed: " <> showEText ent <> " with " <> field <> "=" <> value <> " already exists"

invalidForeignMsg :: T.Text -> T.Text -> T.Text
invalidForeignMsg field value = "Failed: " <> field <> " has an invalid value of " <> value

tagNotFoundMsg :: T.Text -> T.Text
tagNotFoundMsg tag = "Failed: no tag found with id = " <> tag

okCreated :: T.Text -> Int -> Response
okCreated msg id = Response NHT.ok200 val
  where val = Ae.toJSON $ Result True (Just msg) (Just $ Ae.toJSON id)

okDeleted :: (Ae.ToJSON a) => T.Text -> a -> Response
okDeleted msg id = Response NHT.ok200 val
  where val = Ae.toJSON $ Result True (Just msg) (Just $ Ae.toJSON id)

ok :: T.Text -> Ae.Value -> Response
ok text res = Response NHT.ok200 val
  where val = Ae.toJSON $ Result True (Just text) (Just res)

errR :: T.Text -> Ae.Value
errR t = Ae.toJSON $ Result False (Just t) Nothing

bad = Response NHT.status400 . errR



unauthorized = Response NHT.unauthorized401 . errR 

notFound = Response NHT.status404 . errR

internal = Response NHT.internalServerError500 . errR


data Response = Response {
    _r_status :: NHT.Status,
    _r_message :: Ae.Value
    } deriving (Show, Eq)


data Result = Result {
    _ok :: Bool,
    message :: Maybe T.Text,
    result :: Maybe Ae.Value
    } deriving (Show, Eq, Generic, Ae.ToJSON)




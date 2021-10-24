{-# LANGUAGE DeriveAnyClass #-}

module Result where

import qualified Data.Aeson as Ae (Value, ToJSON(..))
import qualified Data.Text as T
import GHC.Generics
import qualified Network.HTTP.Types as NHT
import Types

successGet, successGetProfile, successNewToken :: T.Text
successGet = "Success"

successGetProfile = "Got profile successfully"

successNewToken = "Got token successfully"

forbidden, badInsert, idInResult, unauthorizedMsg :: T.Text
forbidden =
   "Access only for administrators, sending 404 invalid endpoint."

badInsert = "Bad insertion"

idInResult = "id is int \"result\" field"

unauthorizedMsg = "Unauthorized, use /auth"

invalidPasswordMsg, invalidLoginMsg :: T.Text
invalidPasswordMsg = "Invalid password"

invalidLoginMsg = "Invalid login"

invalidEndpointMsg, internalErrorMsg, notAnAuthorMsg ::
      T.Text
invalidEndpointMsg = "Invalid endpoint"

internalErrorMsg = "Internal error"

notAnAuthorMsg = "Not an author" :: T.Text

createdMsg :: Entity -> T.Text
createdMsg ent =
   let enttext = showEText ent
    in "Successfully created " <>
       enttext <>
       ", " <> enttext <> "_id is in \"result\" field"

editedMsg :: Entity -> T.Text
editedMsg ent =
   let enttext = showEText ent
    in "Successfully edited " <>
       enttext <>
       ", " <> enttext <> "_id is in \"result\" field"

deletedMsg :: Entity -> T.Text
deletedMsg ent =
   let enttext = showEText ent
    in "Successfully deleted " <>
       enttext <>
       ", " <> enttext <> "_id is in \"result\" field"

entityNotFoundMsg :: Entity -> T.Text
entityNotFoundMsg ent =
   let enttext = showEText ent
    in enttext <> " not found"

alreadyInUseMsg :: Entity -> T.Text -> T.Text -> T.Text
alreadyInUseMsg ent field value =
   "Failed: " <>
   showEText ent <>
   " with " <> field <> "=" <> value <> " already exists"

invalidForeignMsg :: T.Text -> T.Text -> T.Text
invalidForeignMsg field value =
   "Failed: " <>
   field <> " has an invalid value of " <> value

tagNotFoundMsg :: T.Text -> T.Text
tagNotFoundMsg tag =
   "Failed: no tag found with id = " <> tag

okCreated :: T.Text -> Int -> Response
okCreated msg eid = Response NHT.ok200 val
  where
    val =
       Ae.toJSON $
       Result True (Just msg) (Just $ Ae.toJSON eid)

okDeleted :: (Ae.ToJSON a) => T.Text -> a -> Response
okDeleted msg eid = Response NHT.ok200 val
  where
    val =
       Ae.toJSON $
       Result True (Just msg) (Just $ Ae.toJSON eid)

ok :: T.Text -> Ae.Value -> Response
ok text res = Response NHT.ok200 val
  where
    val = Ae.toJSON $ Result True (Just text) (Just res)

errR :: T.Text -> Ae.Value
errR t = Ae.toJSON $ Result False (Just t) Nothing

bad, unauthorized, notFound, internal :: T.Text -> Response
bad = Response NHT.status400 . errR

unauthorized = Response NHT.unauthorized401 . errR

notFound = Response NHT.status404 . errR

internal = Response NHT.internalServerError500 . errR

data Response =
   Response
      { _r_status :: NHT.Status
      , _r_message :: Ae.Value
      }
   deriving (Show, Eq)

data Result =
   Result
      { _ok :: Bool
      , message :: Maybe T.Text
      , result :: Maybe Ae.Value
      }
   deriving (Show, Eq, Generic, Ae.ToJSON)

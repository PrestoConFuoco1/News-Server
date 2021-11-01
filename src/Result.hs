{-# LANGUAGE DeriveAnyClass #-}

module Result
    ( module Result
    ) where

import qualified Data.Aeson as Ae (ToJSON(..), Value)
import qualified Data.Text as Text
import GHC.Generics
import qualified Network.HTTP.Types as NHT
import qualified Types as T

successGet, successGetProfile, successNewToken :: Text.Text
successGet = "Success"

successGetProfile = "Got profile successfully"

successNewToken = "Got token successfully"

forbidden, badInsert, idInResult, unauthorizedMsg :: Text.Text
forbidden =
    "Access only for administrators, sending 404 invalid endpoint."

badInsert = "Bad insertion"

idInResult = "id is int \"result\" field"

unauthorizedMsg = "Unauthorized, use /auth"

invalidPasswordMsg, invalidLoginMsg :: Text.Text
invalidPasswordMsg = "Invalid password"

invalidLoginMsg = "Invalid login"

invalidEndpointMsg, internalErrorMsg, notAnAuthorMsg :: Text.Text
invalidEndpointMsg = "Invalid endpoint"

internalErrorMsg = "Internal error"

notAnAuthorMsg = "Not an author" :: Text.Text

createdMsg :: T.Entity -> Text.Text
createdMsg ent =
    let enttext = T.showEText ent
     in "Successfully created " <>
        enttext <> ", " <> enttext <> "_id is in \"result\" field"

editedMsg :: T.Entity -> Text.Text
editedMsg ent =
    let enttext = T.showEText ent
     in "Successfully edited " <>
        enttext <> ", " <> enttext <> "_id is in \"result\" field"

deletedMsg :: T.Entity -> Text.Text
deletedMsg ent =
    let enttext = T.showEText ent
     in "Successfully deleted " <>
        enttext <> ", " <> enttext <> "_id is in \"result\" field"

entityNotFoundMsg :: T.Entity -> Text.Text
entityNotFoundMsg ent =
    let enttext = T.showEText ent
     in enttext <> " not found"

alreadyInUseMsg :: T.Entity -> Text.Text -> Text.Text -> Text.Text
alreadyInUseMsg ent field value =
    T.showEText ent <>
    " with " <> field <> "=" <> value <> " already exists"

invalidForeignMsg :: Text.Text -> Text.Text -> Text.Text
invalidForeignMsg field value =
    field <> " has an invalid value of " <> value

constraintViolatedMsg ::
       Text.Text -> Text.Text -> Text.Text -> Text.Text
constraintViolatedMsg field value description =
    field <>
    " has an invalid value of " <>
    value <> "; reason: " <> description

tagNotFoundMsg :: Text.Text -> Text.Text
tagNotFoundMsg tag = "Failed: no tag found with id = " <> tag

okCreated :: Text.Text -> Int -> Response
okCreated msg eid = Response NHT.ok200 val
  where
    val = Ae.toJSON $ Result True (Just msg) (Just $ Ae.toJSON eid)

okDeleted :: (Ae.ToJSON a) => Text.Text -> a -> Response
okDeleted msg eid = Response NHT.ok200 val
  where
    val = Ae.toJSON $ Result True (Just msg) (Just $ Ae.toJSON eid)

ok :: Text.Text -> Ae.Value -> Response
ok text res = Response NHT.ok200 val
  where
    val = Ae.toJSON $ Result True (Just text) (Just res)

errR :: Text.Text -> Ae.Value
errR t = Ae.toJSON $ Result False (Just t) Nothing

bad, unauthorized, notFound, internal :: Text.Text -> Response
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
        , message :: Maybe Text.Text
        , result :: Maybe Ae.Value
        }
  deriving (Show, Eq, Generic, Ae.ToJSON)


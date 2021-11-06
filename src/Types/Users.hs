{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Users
    ( UserId
    , CreateUser(..)
    , DeleteUser(..)
    , GetProfile(..)
    , Authenticate(..)
    , ActionUsers
    ) where

import qualified Data.Text as Text
import Data.Void
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Types.Common (CRUD)

type UserId = Int

type ActionUsers = CRUD CreateUser GetProfile Void DeleteUser

data CreateUser =
    CreateUser
        { cuLogin :: Text.Text
        , cuPassHash :: Text.Text
        , cuFirstName :: Text.Text
        , cuLastName :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data GetProfile =
    GetProfile
  deriving (Show, Eq, Generic, PS.ToRow)
  deriving GP.PrettyShow via GP.Showable GetProfile

newtype DeleteUser =
    DeleteUser
        { duUserId :: UserId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data Authenticate =
    Authenticate
        { auLogin :: Text.Text
        , auPassHash :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)


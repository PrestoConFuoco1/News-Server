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
import Types.Common (CRUD, Paginated)

type UserId = Int

type ActionUsers = CRUD CreateUser GetProfile Void DeleteUser

data CreateUser =
    CreateUser
        { _cu_login :: Text.Text
        , _cu_passHash :: Text.Text
        , _cu_firstName :: Text.Text
        , _cu_lastName :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data GetProfile =
    GetProfile
  deriving (Show, Eq, Generic, PS.ToRow)
  deriving GP.PrettyShow via GP.Showable GetProfile

newtype DeleteUser =
    DeleteUser
        { _du_userId :: UserId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data Authenticate =
    Authenticate
        { _au_login :: Text.Text
        , _au_passHash :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)


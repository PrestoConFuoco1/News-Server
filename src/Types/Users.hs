{-# LANGUAGE DeriveAnyClass #-}

module Types.Users where

import qualified Data.Text as T
import Data.Void
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import Types.Common

type UserId = Int

type ActionUsers
    = CRUD CreateUser GetProfile Void DeleteUser

data CreateUser =
   CreateUser
      { _cu_login :: T.Text
      , _cu_passHash :: T.Text
      , _cu_firstName :: T.Text
      , _cu_lastName :: T.Text
      }
   deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data GetProfile =
   GetProfile
   deriving (Show, Eq, Generic, PS.ToRow)

instance GP.PrettyShow GetProfile where
   prettyShow = GP.LStr . show

data DeleteUser =
   DeleteUser
      { _du_userId :: UserId
      }
   deriving (Show, Eq, Generic, GP.PrettyShow, PS.ToRow)

data Authenticate =
   Authenticate
      { _au_login :: T.Text
      , _au_passHash :: T.Text
      }
   deriving (Show, Eq, Generic, GP.PrettyShow)

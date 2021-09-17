{-# LANGUAGE DeriveAnyClass #-}
module Action.Users.Types where


import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import Data.Void
import Action.Common

type ActionUsers = CRUD CreateUser Void Void Void
  
data CreateUser = CreateUser {
    _cu_login :: T.Text,
    _cu_passHash :: T.Text,
    _cu_firstName :: T.Text,
    _cu_lastName  :: T.Text
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)



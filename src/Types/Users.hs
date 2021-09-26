{-# LANGUAGE DeriveAnyClass #-}
module Action.Users where


import Action.Utils

import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import Data.Void
import Action.Common

type ActionUsers = CRUD CreateUser GetProfile Void DeleteUser
  
data CreateUser = CreateUser {
    _cu_login :: T.Text,
    _cu_passHash :: T.Text,
    _cu_firstName :: T.Text,
    _cu_lastName  :: T.Text
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data GetProfile = GetProfile
    deriving (Show, Generic, PS.ToRow)

instance GP.PrettyShow GetProfile where
    prettyShow = GP.LStr . show

data DeleteUser = DeleteUser {
    _du_userId :: Int
    } deriving (Show, Generic, GP.PrettyShow, PS.ToRow)

data Authenticate = Authenticate {
    _au_login :: T.Text,
    _au_passHash :: T.Text
    } deriving (Show, Generic, GP.PrettyShow)



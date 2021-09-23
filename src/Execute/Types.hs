{-# LANGUAGE DeriveAnyClass #-}
module Execute.Types where


import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.ToRow as PSR

import qualified Data.Text as T
import qualified Types as Ty

import qualified Network.HTTP.Types as NHT
import qualified Data.Aeson as Ae
import GHC.Generics

data Response = Response {
    _r_status :: NHT.Status,
    _r_message :: Ae.Value
    } deriving (Show, Eq)


data Result = Result {
    _ok :: Bool,
    message :: Maybe T.Text,
    result :: Maybe Ae.Value
    } deriving (Show, Eq, Generic, Ae.ToJSON)



data WithUser a = WithUser {
    _wu_userId :: Ty.User,
    _wu_action :: a
    } deriving (Show, Generic)

data WithAuthor a = WithAuthor {
    _wa_authorId :: Int,
    _wa_action :: a
    } deriving (Show, Generic)

{-
instance PSR.ToRow a => PSR.ToRow (WithUser a) where
    toRow (WithUser int x) = PSR.toRow (PS.Only int PS.:. x)


instance PSR.ToRow a => PSR.ToRow (WithAuthor a) where
    toRow (WithAuthor int x) = PSR.toRow (PS.Only int PS.:. x)
-}

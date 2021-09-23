{-# LANGUAGE DeriveAnyClass #-}
module Action.Types where

import GHC.Generics
import qualified Data.Aeson as Ae (decode, Value)

import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Aeson as Ae (decode, Value)
import qualified Data.ByteString.Lazy as BSL (fromStrict, unpack, ByteString)
import qualified Data.ByteString as BS
import qualified Data.Time as Time
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS



type Token = T.Text

data WhoWhat a = WhoWhat {
    _ww_token  :: Maybe Token,
    --_ww_action :: Action
    _ww_action :: a
    } deriving (Show, Generic) 

--data GetComments = GetComments PostId
--    deriving (Show, Generic)


{-# LANGUAGE DeriveAnyClass #-}
module Action.Common where


import qualified Data.ByteString as BS
import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.HashMap.Strict as HS (HashMap, fromList, lookup)



type Query = HS.HashMap BS.ByteString BS.ByteString


data ActionError = EInvalidEndpoint | ERequiredFieldMissing BS.ByteString
    deriving (Show, Generic)

data CRUD c r u d = Create c | Read r | Update u | Delete d
    deriving (Generic, GP.PrettyShow)

-- invalidEP = AError EInvalidEndpoint

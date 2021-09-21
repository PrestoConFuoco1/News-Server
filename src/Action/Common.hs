{-# LANGUAGE DeriveAnyClass #-}
module Action.Common where


import qualified Data.ByteString as BS
import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.HashMap.Strict as HS (HashMap, fromList, lookup)
import Control.Monad.Reader


type Query = HS.HashMap BS.ByteString BS.ByteString


data ActionError = EInvalidEndpoint
    | ERequiredFieldMissing Bool BS.ByteString
    | EInvalidFieldValue Bool BS.ByteString
    -- here Bool is needed because we need to know whether 
    -- the user can access this endpoint or not
    deriving (Show, Generic)

data CRUD c r u d = Create c | Read r | Update u | Delete d
    deriving (Generic, GP.PrettyShow, Show)

data Env = Env {
    Bool
    } (Show, Eq)

type Router e a = ReaderT e (Either ActionError a)


-- invalidEP = AError EInvalidEndpoint

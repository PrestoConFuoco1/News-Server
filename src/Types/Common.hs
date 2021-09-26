{-# LANGUAGE
             GeneralizedNewtypeDeriving  #-}
module Types.Common (
CRUD(..), Paginated(..)
) where


import qualified Data.ByteString as BS
import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.HashMap.Strict as HS (HashMap, fromList, lookup)
import Control.Monad.Reader
import Data.Bifunctor (first)
--import Control.Applicativ

data CRUD c r u d = Create c | Read r | Update u | Delete d
    deriving (Generic, Show)
instance (GP.PrettyShow c, GP.PrettyShow r,
          GP.PrettyShow u, GP.PrettyShow d) => GP.PrettyShow (CRUD c r u d)

-- invalidEP = AError EInvalidEndpoint

data Paginated a = Paginated {
    _pag_page :: Int,
    _pag_size :: Int,
    _pag_data :: a
    } deriving (Show, Generic)

instance GP.PrettyShow a => GP.PrettyShow (Paginated a) 



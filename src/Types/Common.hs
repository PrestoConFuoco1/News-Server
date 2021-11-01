module Types.Common
    ( CRUD(..)
    , Paginated(..)
    , foldCRUD
    ) where

import GHC.Generics
import qualified GenericPretty as GP
import Prelude hiding (Read, read)

data CRUD c r u d
    = Create c
    | Read r
    | Update u
    | Delete d
  deriving (Show, Eq, Generic)

foldCRUD ::
       CRUD c r u d
    -> (c -> a)
    -> (r -> a)
    -> (u -> a)
    -> (d -> a)
    -> a
foldCRUD x create read update delete =
    case x of
        Create c -> create c
        Read r -> read r
        Update u -> update u
        Delete d -> delete d

instance ( GP.PrettyShow c
         , GP.PrettyShow r
         , GP.PrettyShow u
         , GP.PrettyShow d
         ) =>
         GP.PrettyShow (CRUD c r u d)

data Paginated a =
    Paginated
        { _pag_page :: Int
        , _pag_size :: Int
        , _pag_data :: a
        }
  deriving (Show, Eq, Generic)

instance GP.PrettyShow a => GP.PrettyShow (Paginated a)

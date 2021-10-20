{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Common
   ( CRUD(..)
   , Paginated(..)
   ) where

import GHC.Generics
import qualified GenericPretty as GP

data CRUD c r u d
   = Create c
   | Read r
   | Update u
   | Delete d
   deriving (Show, Eq, Generic)

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

instance GP.PrettyShow a =>
         GP.PrettyShow (Paginated a)

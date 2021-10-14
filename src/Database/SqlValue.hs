{-# LANGUAGE ExistentialQuantification #-}
module Database.SqlValue where

import qualified Database.PostgreSQL.Simple.ToField as PSF
import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Time as Time
import GenericPretty

data SqlValue = forall a. (PSF.ToField a, Show a) => SqlValue a

instance PSF.ToField SqlValue where
    toField (SqlValue x) = PSF.toField x

instance Show SqlValue where
    show (SqlValue x) = show x

instance PrettyShow SqlValue where
    prettyShow (SqlValue x) = LStr $ show x

{-# LANGUAGE ExistentialQuantification #-}
module SqlValue where

import qualified Database.PostgreSQL.Simple.ToField as PSF
import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Time as Time


{-
-}
data SqlValue = SqlDate Time.Day
              | SqlTextL TL.Text
              | SqlText T.Text
              | SqlArray (PSTy.PGArray Int)
              | SqlInt Int
              | SqlMaybeText (Maybe T.Text)
              | SqlMaybeTextList 
    deriving (Show)

instance PSF.ToField SqlValue where
    toField (SqlDate day) = PSF.toField day
    toField (SqlText text) = PSF.toField text
    toField (SqlArray ints) = PSF.toField ints
    toField (SqlInt int) = PSF.toField int

data SqlValue' = forall a. (PSF.ToField a) => SqlValue' a

instance PSF.ToField SqlValue' where
    toField (SqlValue' x) = PSF.toField x

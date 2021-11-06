{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module DerivingJSON where

import qualified Data.Aeson.Types as Ae
import Data.Char (isLower)
import GHC.Generics
import Utils (unCap)

newtype DropLowerUncap a =
    DropLowerUncap
        { unDropLowerUncap :: a
        }

instance (Generic a, Ae.GFromJSON Ae.Zero (Rep a)) =>
         Ae.FromJSON (DropLowerUncap a) where
    parseJSON =
        fmap DropLowerUncap .
        Ae.genericParseJSON
            (Ae.defaultOptions
                 { Ae.fieldLabelModifier =
                       dropLowerUncap
                 })

instance (Generic a, Ae.GToJSON' Ae.Value Ae.Zero (Rep a)) =>
         Ae.ToJSON (DropLowerUncap a) where
    toJSON =
        Ae.genericToJSON
            (Ae.defaultOptions
                 { Ae.fieldLabelModifier =
                       --Ae.camelTo2 '_' . dropWhile isLower
                       dropLowerUncap
                 }) .
        unDropLowerUncap

dropLowerUncap :: String -> String
dropLowerUncap = unCap . dropWhile isLower

newtype RemovePrefix a =
    RemovePrefix
        { unRemovePrefix :: a
        }

instance (Generic a, Ae.GFromJSON Ae.Zero (Rep a)) =>
         Ae.FromJSON (RemovePrefix a) where
    parseJSON =
        fmap RemovePrefix .
        Ae.genericParseJSON
            Ae.defaultOptions
                {Ae.fieldLabelModifier = removeTwoUnderscores}

instance (Generic a, Ae.GToJSON' Ae.Value Ae.Zero (Rep a)) =>
         Ae.ToJSON (RemovePrefix a) where
    toJSON =
        Ae.genericToJSON
            (Ae.defaultOptions
                 {Ae.fieldLabelModifier = removeTwoUnderscores}) .
        unRemovePrefix

removeTwoUnderscores :: String -> String
removeTwoUnderscores x@('_':ys) =
    case dropWhile (/= '_') ys of
        ('_':zs) -> zs
        _ -> x
removeTwoUnderscores x = x

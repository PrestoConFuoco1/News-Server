{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module DerivingJSON where

import qualified Data.Aeson.Types as Ae
import Data.Char (isLower)
import GHC.Generics

newtype BotSelectorModifier a =
    BotSelectorModifier
        { unBotSelectorModifier :: a
        }

instance (Generic a, Ae.GFromJSON Ae.Zero (Rep a)) =>
         Ae.FromJSON (BotSelectorModifier a) where
    parseJSON =
        fmap BotSelectorModifier .
        Ae.genericParseJSON
            (Ae.defaultOptions
                 { Ae.fieldLabelModifier =
                       Ae.camelTo2 '_' . dropWhile isLower
                 })

instance (Generic a, Ae.GToJSON' Ae.Value Ae.Zero (Rep a)) =>
         Ae.ToJSON (BotSelectorModifier a) where
    toJSON =
        Ae.genericToJSON
            (Ae.defaultOptions
                 { Ae.fieldLabelModifier =
                       Ae.camelTo2 '_' . dropWhile isLower
                 }) .
        unBotSelectorModifier


newtype RemovePrefix a = RemovePrefix { unRemovePrefix :: a } 
  
instance (Generic a, Ae.GFromJSON Ae.Zero (Rep a)) => Ae.FromJSON (RemovePrefix a) where 
     parseJSON = fmap RemovePrefix . Ae.genericParseJSON 
         Ae.defaultOptions {Ae.fieldLabelModifier = removeTwoUnderscores}

instance (Generic a, Ae.GToJSON' Ae.Value Ae.Zero (Rep a)) =>
         Ae.ToJSON (RemovePrefix a) where
    toJSON =
        Ae.genericToJSON
            (Ae.defaultOptions
                 { Ae.fieldLabelModifier =
                       removeTwoUnderscores
                 }) .
        unRemovePrefix

removeTwoUnderscores :: String -> String
removeTwoUnderscores x@('_':ys) =
    case dropWhile (/= '_') ys of
        ('_':zs) -> zs
        _ -> x
removeTwoUnderscores x = x

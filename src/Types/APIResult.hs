{-# LANGUAGE ExistentialQuantification #-}

module Types.APIResult where

import qualified Data.Text as T
import Types.Entity

import GHC.Generics
import GenericPretty
import Prelude as P
import Utils

data APIResult
   = RGet RGettable
   | RGetUser User
   | RGetToken T.Text
   | RCreated Entity Int
   | REdited Entity Int
   | RDeleted Entity Int
   | RNotFound Entity
   | RAlreadyInUse Entity T.Text T.Text
   | RInvalidForeign Entity T.Text T.Text
   | RInvalidTag T.Text
   deriving (Show, Generic)

logResult :: APIResult -> T.Text
logResult (RGet (RGettable xs)) =
   "fetched " <> showText (length xs) <> " entities"
logResult (RGetUser u) = textPretty u
logResult x = showText x

data RGettable =
   forall a. (Gettable a, Show a) =>
             RGettable [a]

instance Show RGettable where
   show (RGettable xs) = show xs

instance PrettyShow RGettable where
   prettyShow (RGettable xs) = prettyShow xs

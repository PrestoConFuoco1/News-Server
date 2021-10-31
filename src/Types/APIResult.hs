{-# LANGUAGE ExistentialQuantification #-}

module Types.APIResult
    ( APIResult(..)
    , logResult
    , RGettable(..)
    ) where

import qualified Data.Text as T
import Types.Entity (Entity, Gettable, User)

import GHC.Generics
import qualified GenericPretty as GP
import Prelude as P
import qualified Utils as S

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
    "fetched " <> S.showText (length xs) <> " entities"
logResult (RGetUser u) = GP.textPretty u
logResult x = S.showText x

data RGettable =
    forall a. (Gettable a, Show a) =>
              RGettable [a]

instance Show RGettable where
    show (RGettable xs) = show xs

instance GP.PrettyShow RGettable where
    prettyShow (RGettable xs) = GP.prettyShow xs

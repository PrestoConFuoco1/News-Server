{-# LANGUAGE ExistentialQuantification #-}
module Types.APIResult where

import Types.Entity
import qualified Data.Text as T
--import Types.APIErrors
import GHC.Generics
import GenericPretty
import Prelude as P
import Utils


data APIResult =
      RGet RGettable
    | RGetUser User
    | RGetToken T.Text
    | RCreated Entity Int -- id of the created entity
    | REdited  Entity Int -- id of the edited entity
    | RDeleted Entity Int
    | RNotFound Entity
    | RAlreadyInUse Entity T.Text T.Text
    | RInvalidForeign Entity T.Text T.Text
    | RInvalidTag T.Text
 --   | RError Ex.ServerException
    deriving (Show, Generic)

logResult :: APIResult -> T.Text
--logResult (RGet x) = showText x
logResult (RGet (RGettable xs)) = "fetched " <> showText (length xs) <> " entities"
logResult (RGetUser u) = textPretty u
logResult x = showText x

data RGettable = forall a. (Gettable a, Show a) => RGettable [a]

instance Show RGettable where
    show (RGettable xs) = show xs

instance PrettyShow RGettable where
    prettyShow (RGettable xs) = prettyShow xs
--data SqlValue = forall a. (PSF.ToField a) => SqlValue a

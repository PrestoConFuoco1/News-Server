{-# LANGUAGE ExistentialQuantification #-}
module Types.APIResult where

import Types.Entity
import Data.Text
--import Types.APIErrors




data APIResult =
      RGet RGettable
    | RGetUser User
    | RGetToken Text
    | RCreated Entity Int -- id of the created entity
    | REdited  Entity Int -- id of the edited entity
    | RDeleted Entity Int
    | RNotFound Entity
    | RAlreadyInUse Entity Text Text
    | RInvalidForeign Entity Text Text
    | RInvalidTag Text
 --   | RError Ex.ServerException


data RGettable = forall a. (Gettable a) => RGettable [a]
--data SqlValue = forall a. (PSF.ToField a) => SqlValue a

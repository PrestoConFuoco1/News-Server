{-# LANGUAGE ExistentialQuantification #-}
module Types.APIResult where


import Data.Text
import Types.APIErrors


data APIResult =
      RGet RGettable
      GetUser User
    | Created Text Int -- id of the created entity
    | Edited  Text Int -- id of the edited entity
    | AlreadyInUse
    

data RGettable = forall a. (Gettable a) => RGettable [a]
--data SqlValue = forall a. (PSF.ToField a) => SqlValue a

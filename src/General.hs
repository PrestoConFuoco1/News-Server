{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, TypeFamilies, UndecidableInstances #-}

module General where


import GHC.Generics
import qualified Database.PostgreSQL.Simple.FromField as PSF
import qualified Database.PostgreSQL.Simple.Types as PST
import Type.Reflection



instance (Typeable a, PSF.FromField a) => PSF.FromField [a] where
    fromField f b = fmap PST.fromPGArray $ PSF.fromField f b

defaultModifier ('_':xs) = case dropWhile (/= '_') xs of
    ('_':ys) -> ys
    _ -> xs
defaultModifier xs = xs

--data NumWrap = forall a . Num a => NumWrap a

    

{-
data Create 
data Update 
data Filter

-- | Аналог функции `elem` на уровне типов
type family Contains a as where
  Contains a (a ': as) = 'True 
  -- | Для поднятия конструкторов типа Bool на уровень 
  -- типов понадобится DataKinds
  Contains b (a ': as) = Contains b as
  Contains a '[]       = 'False

-- | Аналог ifThenElse на уровне типов
type family If c t f where 
  If 'True  t f = t
  If 'False t f = f

data Immutable
data NotForSearch

type family Field action (modifiers :: [*]) a :: *

type instance Field Create constraints a = a 

type instance Field Update constraints a = 
  --_если_ (список модификаторов содержит Immutable) _тогда_ () _иначе_ (Maybe a)
    If     (Contains Immutable constraints)                  ()         (Maybe a)

type instance Field Filter constraints a =
    If (Contains NotForSearch constraints) () [a]




data User a = User
  { login :: Field a '[Immutable]    String
  , email :: Field a '[]             String
  , about :: Field a '[NotForSearch] String }
-}

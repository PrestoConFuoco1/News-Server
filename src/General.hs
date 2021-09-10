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

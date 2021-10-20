{-# LANGUAGE
    TypeFamilies
    , FlexibleContexts
    , FlexibleInstances
    , DefaultSignatures
    #-}

module GenericPretty where

import GHC.Generics
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Lazy as TL (Text, unpack)
import Data.Void
import Data.Aeson.Types
import Data.Aeson (encode)
import Data.Char
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString as B
import qualified Data.Time as Time
import qualified Utils as S

enclose, encloseSq :: String -> String
enclose s = '{' : s ++ "}"
encloseSq s = '[' : s ++ "]"
------

data OptionsL = OptionsL {
        labelModifier :: String -> String,
        consModifier :: String -> String
    }

defaultOptionsL :: OptionsL
defaultOptionsL = OptionsL {
    labelModifier = defaultModif,
    consModifier = defaultConsModif
    }

defaultModif :: String -> String
defaultModif x@('_':ys) =
    case dropWhile (/= '_') ys of
        ('_':zs) -> zs
        _ -> x
defaultModif x = x

defaultConsModif :: String -> String
defaultConsModif = id

defaultConsModif' :: String -> String
defaultConsModif' (x:xs)
    | isUpper x = case dropWhile (not . isUpper) xs of
        [] -> (x:xs)
        y  -> y
    | otherwise = (x:xs)
defaultConsModif' x = x

----------------------

data LayoutUnit = LayoutUnit String LayoutValue
    deriving (Show, Eq)

data LayoutValue = LStr String | LLay String Layout | LEmpty | LJSON String
    deriving (Show, Eq)

newtype Layout = Layout [LayoutUnit]
    deriving (Show, Eq)


lconcat :: Layout -> Layout -> Layout
lconcat (Layout l) (Layout r) = Layout $ l ++ r

defaultIndent :: Int
defaultIndent = 4

numToIndent :: Int -> String
numToIndent ind = replicate (ind * defaultIndent) ' '

defaultWidth :: Int
defaultWidth = 80

splitToFixedWidth :: Int -> String -> [String]
splitToFixedWidth ind s =
    let width = defaultWidth - ind * defaultIndent
        res = splitToFixedWidth' width s
    in  res

splitToFixedWidth' :: Int -> String -> [String]
splitToFixedWidth' _   [] = []
splitToFixedWidth' wid s  =
    let splitted = splitAt wid s
    in  case splitted of
            (pref, suf) -> pref : splitToFixedWidth' wid suf

withIndent :: Int -> String -> String
withIndent ind str = numToIndent ind ++ str

defaultPretty :: (PrettyShow a) => a -> String
defaultPretty x = prettyValue 0 (prettyShow x)

textPretty :: (PrettyShow a) => a -> T.Text
textPretty = T.pack . defaultPretty

prettyUnit :: Int -> LayoutUnit -> String
prettyUnit _ (LayoutUnit _ (LEmpty)) = ""
prettyUnit ind (LayoutUnit s val) =
    withIndent ind $ s ++ ": " ++ prettyValue ind val

prettyValue :: Int -> LayoutValue -> String
prettyValue _   (LStr s) = s ++ "\n"
prettyValue ind (LLay typ ls) = typ ++ "\n" ++ prettyLayout (ind + 1) ls
prettyValue ind (LJSON s) = ('\n' :) $ unlines $ map (withIndent $ ind + 1) $ splitToFixedWidth ind s
prettyValue _   LEmpty    = "empty\n"

prettyLayout :: Int -> Layout -> String
prettyLayout ind (Layout ls) = concat $ map (prettyUnit ind) ls

class PrettyShow a where
    prettyShow :: a -> LayoutValue

    default prettyShow :: (Generic a, GPrettyShow (Rep a)) => a -> LayoutValue
    prettyShow = genericPrettyShow defaultOptionsL

genericPrettyShow :: (Generic a, GPrettyShow (Rep a)) => OptionsL -> a -> LayoutValue
genericPrettyShow opts = gprettyShow opts . from
--------------------------------------------------------------------

newtype StrWrap = StrWrap { unStrWrap :: String } 

instance PrettyShow StrWrap where
    prettyShow s = LStr $ unStrWrap s

instance PrettyShow Int where
    prettyShow = LStr . show

instance PrettyShow Integer where
    prettyShow = LStr . show

instance PrettyShow Double where
    prettyShow = LStr . show

instance PrettyShow T.Text where
    --prettyShow = LStr . T.unpack
    prettyShow = LStr . show

instance PrettyShow TL.Text where
    prettyShow = LStr . show

instance PrettyShow B.ByteString where
    prettyShow = LStr . show

instance PrettyShow Value where
    --prettyShow val = LStr $ "JSON value"
    prettyShow val = LJSON $ TL.unpack $ decodeUtf8 $ encode val

instance PrettyShow Bool where
    prettyShow = LStr . show

instance PrettyShow Time.Day where
    --prettyShow = LStr . Time.formatTime Time.defaultTimeLocale "%F"
    prettyShow = LStr . S.showDay

instance PrettyShow Void where
    prettyShow _ = LStr ""



instance (PrettyShow a) => PrettyShow (Maybe a) where
    prettyShow (Just x) = prettyShow x
    prettyShow (Nothing) = LEmpty

instance (PrettyShow a) => PrettyShow [a] where
    prettyShow [] = LEmpty
    prettyShow xs = LLay "{Array}" $ Layout $ foldr f [] $ zip [0::Integer ..] xs
      where f (n, x) acc = LayoutUnit (encloseSq $ show n) (prettyShow x) : acc


------------------------------------------

class GPrettyShow f where
    gprettyShow :: OptionsL -> f a -> LayoutValue

class GPrettyShowAux f where
    gprettyShowAux :: OptionsL -> f a -> Layout

class GPrettyShowIgnoreConstr f where
    gprettyShowIgnoreConstr :: OptionsL -> f a -> LayoutValue

--------------------------------------------------------------------

instance (GPrettyShow f) => GPrettyShow (D1 d f) where
    gprettyShow opts (M1 x) = gprettyShow opts x
-- Если от нас сразу требуют LayoutValue, то тип данных не нужен

instance (GPrettyShowIgnoreConstr f, GPrettyShowIgnoreConstr g) => GPrettyShow ((:+:) f g) where
    gprettyShow opts x = gprettyShowIgnoreConstr opts x

instance (GPrettyShowIgnoreConstr f, GPrettyShowIgnoreConstr g) => GPrettyShowIgnoreConstr ((:+:) f g) where
    gprettyShowIgnoreConstr opts (L1 x) = gprettyShowIgnoreConstr opts x
    gprettyShowIgnoreConstr opts (R1 x) = gprettyShowIgnoreConstr opts x

instance (GPrettyShowIgnoreConstr f, Constructor c) => GPrettyShowIgnoreConstr (C1 c f) where
    gprettyShowIgnoreConstr opts (M1 x) = gprettyShowIgnoreConstr opts x

instance (GPrettyShow f) => GPrettyShowIgnoreConstr (S1 c f) where
    gprettyShowIgnoreConstr opts (M1 x) = gprettyShow opts x


instance (Constructor c, GPrettyShowAux f) => GPrettyShow (C1 c f) where
    gprettyShow opts m@(M1 x) = LLay (enclose $ consModifier opts $ conName m) $ gprettyShowAux opts x

instance (PrettyShow c) => GPrettyShow (Rec0 c) where
    gprettyShow opts (K1 x) = prettyShow x

--------------------------------------------------------------------

instance (GPrettyShowAux f, GPrettyShowAux g) => GPrettyShowAux ((:*:) f g) where
    gprettyShowAux opts (x :*: y) = gprettyShowAux opts x `lconcat` gprettyShowAux opts y

instance (Selector s, GPrettyShow f) => GPrettyShowAux (S1 s f) where
    gprettyShowAux opts s@(M1 x) = Layout [LayoutUnit (labelModifier opts $ selName s) (gprettyShow opts x)]


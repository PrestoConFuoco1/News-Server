module Utils where

import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple.FromField as PSF
import qualified Database.PostgreSQL.Simple.Types as PST
import Debug.Trace
import GHC.Arr
import System.Random (randomRIO)
import Text.Regex.PCRE ((=~))
import Type.Reflection (Typeable)
import qualified Data.Time.Clock.System as Sys

instance (Typeable a, PSF.FromField a) => PSF.FromField [a] where
    fromField f b = PST.fromPGArray <$> PSF.fromField f b

echo :: Show a => a -> a
echo x = show x `trace` x

defaultModifier :: String -> String
defaultModifier ('_':xs) =
    case dropWhile (/= '_') xs of
        ('_':ys) -> ys
        _ -> xs
defaultModifier xs = xs

showText :: (Show a) => a -> Text.Text
showText = Text.pack . show

test1, test2 :: B.ByteString
test1 = "Key (user_id)=(6666) is not present in table \"users\"."

test2 = "Key (user_id)=(2) already exists."

getPair :: B.ByteString -> Maybe (Text.Text, Text.Text)
getPair str =
    let patt = "\\((\\w+)\\)=\\((\\w+)\\)" :: B.ByteString
     in case str =~ patt of
            (_:y:z:_):_ -> Just (E.decodeUtf8 y, E.decodeUtf8 z)
            _ -> Nothing

unCap :: String -> String
unCap (x:xs) = toLower x : xs
unCap ys = ys

randomString :: Int -> IO String
randomString int = do
    let str = "qwertyuiopasdfghjklzxcvbnm"
        len = length str
        arr = array (1, len) $ zip [1 .. len] str
    xs <- replicateM int $ randomRIO (1, len)
    pure $ map (arr !) xs

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe x nothing just = maybe nothing just x

showDay :: Time.Day -> String
showDay = Time.formatTime Time.defaultTimeLocale "%F"

readDay :: String -> Maybe Time.Day
readDay = Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d"

secondsSinceEpoch :: IO Int
secondsSinceEpoch = fromIntegral . Sys.systemSeconds <$> Sys.getSystemTime

utcTime :: IO Time.UTCTime
utcTime = Sys.systemToUTCTime <$> Sys.getSystemTime

showUTCTime :: Time.UTCTime -> String
showUTCTime = Time.formatTime Time.defaultTimeLocale "%F %T.%4q UTC"

showUTCTimeText = Text.pack . showUTCTime



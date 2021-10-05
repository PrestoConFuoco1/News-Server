module Utils where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Text.Regex.PCRE
import qualified Data.Text.Encoding as E
import Data.Char
import System.Random
import GHC.Arr




showText :: (Show a) => a -> T.Text
showText = T.pack . show


test1 = "Key (user_id)=(6666) is not present in table \"users\"." :: B.ByteString
test2 = "Key (user_id)=(2) already exists." :: B.ByteString

--getPair :: B.ByteString -> Maybe (B.ByteString, B.ByteString)
getPair :: B.ByteString -> Maybe (T.Text, T.Text)
getPair str = let patt = "\\((\\w+)\\)=\\((\\w+)\\)" :: B.ByteString
          in  case str =~ patt of
                (x:y:z:_) : _ -> Just (E.decodeUtf8 y, E.decodeUtf8 z)
                _ -> Nothing

unCap (x:xs) = toLower x : xs
unCap ys = ys


randomString' :: Int -> IO String            
randomString' int = do
        let str = "qwertyuiopasdfghjklzxcvbnm"
            len = length str
            arr = array (1, len) $ zip [1..len] str
        xs <- sequence $ replicate int $ randomRIO (1, len)
        return $ map (arr !) xs


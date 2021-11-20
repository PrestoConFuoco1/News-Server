module Crypto where

import qualified Crypto.Hash as Hash
import qualified Crypto.Random.Types as CRT
import qualified Data.ByteArray.Encoding as DBE
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Utils as U

convertHex :: BS.ByteString -> BS.ByteString
convertHex = DBE.convertToBase DBE.Base16

randomHex :: Int -> IO BS.ByteString
randomHex n = convertHex <$> CRT.getRandomBytes n

randomHexDebug :: Int -> IO (BS.ByteString, BS.ByteString)
randomHexDebug n = do
    rndBS <- CRT.getRandomBytes n
    pure (rndBS, convertHex rndBS)

saltLength :: Int
saltLength = 16

generatePasswordHashWithSalt :: T.Text -> IO T.Text
generatePasswordHashWithSalt pass = do
    salt <- TEnc.decodeUtf8 <$> randomHex saltLength
    pure $ generatePasswordHash pass salt <> salt

generatePasswordHash :: T.Text -> T.Text -> T.Text
generatePasswordHash pass salt =
    let passWithSalt = TEnc.encodeUtf8 $ pass <> salt
        hashed =
            DBE.convertToBase DBE.Base16 $
            Hash.hashWith Hash.SHA256 passWithSalt
     in TEnc.decodeUtf8 hashed

validatePassword :: T.Text -> T.Text -> Bool
validatePassword pass passHashWithSalt =
    let (passHash, salt) =
            splitAtEnd (saltLength * 2) passHashWithSalt
     in case salt of
            "" -> False
            _ -> passHash == generatePasswordHash pass salt

splitAtEnd :: Int -> T.Text -> (T.Text, T.Text)
splitAtEnd n bs = T.splitAt (T.length bs - n) bs

module ExecuteUtils where


import qualified Network.HTTP.Types as NHT
import qualified Data.Aeson as Ae

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import ExecuteTypes

ok = Response NHT.ok200 . msgValue "ok"
bad = Response NHT.status400 . errValue
unauthorized = Response NHT.unauthorized401 . errValue
notFound = Response NHT.status404 . errValue
internal = Response NHT.internalServerError500 . errValue

msgValue :: T.Text -> B.ByteString -> Ae.Value
msgValue field str = Ae.object [(field, Ae.String $ E.decodeUtf8 str)]

errValue str = msgValue "errmsg" str


unauthorizedMsg = "Unauthorized, use /auth"

invalidEndpointMsg = "Invalid endpoint" :: B.ByteString



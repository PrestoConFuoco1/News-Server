
module ExecuteTypes where



import qualified Network.HTTP.Types as NHT
import qualified Data.Aeson as Ae


data Response = Response {
    _r_status :: NHT.Status,
    _r_message :: Ae.Value
    }



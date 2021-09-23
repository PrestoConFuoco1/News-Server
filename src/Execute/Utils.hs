module Execute.Utils where


import qualified Network.HTTP.Types as NHT
import qualified Data.Aeson as Ae

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import Execute.Types (Result(..), Response(..))
import qualified Exceptions as Ex
import MonadTypes
import qualified GenericPretty as GP

validateUnique :: (MonadServer m, GP.PrettyShow a) => m a -> [a] -> m a
validateUnique x [] = x
validateUnique _ [a] = return a
validateUnique _ us  = Ex.invalidUnique us


validateUnique2 :: (MonadServer m) => m a -> m a -> [a] -> m a
validateUnique2 empty toomuch [] = empty
validateUnique2 empty toomuch [a] = return a
validateUnique2 empty toomuch us = toomuch

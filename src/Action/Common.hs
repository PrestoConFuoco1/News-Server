{-# LANGUAGE
             GeneralizedNewtypeDeriving  #-}
module Action.Common (
Query, ActionError(..),
ActionErrorPerms(..), ask,
RoutingEnv(..), Router(..), askAdmin, askHash, routerError,
runRouter, withMaybe, errorOnNothing, renv, pathNotFound
) where


import qualified Data.ByteString as BS
import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.HashMap.Strict as HS (HashMap, fromList, lookup)
import Control.Monad.Reader
import Data.Bifunctor (first)

type Query = HS.HashMap BS.ByteString BS.ByteString

data ActionError = EInvalidEndpoint
    | ERequiredFieldMissing BS.ByteString
    | EInvalidFieldValue BS.ByteString
    deriving (Show, Generic, Eq)

-- invalidEP = AError EInvalidEndpoint

pathNotFound = ActionErrorPerms {
    _ae_admin = False
    , _ae_error = EInvalidEndpoint
    }

data ActionErrorPerms = ActionErrorPerms {
    _ae_admin :: Bool,
    _ae_error :: ActionError
    } deriving (Show, Generic, Eq)

data RoutingEnv = RoutingEnv {
    _re_admin :: Bool,
    _re_hash :: Query
    } deriving (Show, Generic)

instance GP.PrettyShow ActionError where
    prettyShow = GP.LStr . show

instance GP.PrettyShow ActionErrorPerms where
    prettyShow = GP.LStr . show


newtype Router a = Router { unR :: ReaderT RoutingEnv (Either ActionErrorPerms) a }
    deriving (Functor, Applicative, Monad, MonadReader RoutingEnv)

askAdmin :: Router Bool
askAdmin = fmap _re_admin ask

askHash :: Router Query
askHash = fmap _re_hash ask

routerError :: ActionError -> Router a
routerError err = askAdmin >>= \admin ->
    Router $ ReaderT $ \e -> Left $ ActionErrorPerms admin err

runRouter :: RoutingEnv -> Router a -> Either ActionErrorPerms a
runRouter e rt = runReaderT (unR rt) e

withMaybe :: Router a -> Maybe a -> Router a
withMaybe r = maybe r return

errorOnNothing :: ActionError -> Maybe a -> Router a
errorOnNothing e = maybe (routerError e) return

renv b hash = RoutingEnv b hash



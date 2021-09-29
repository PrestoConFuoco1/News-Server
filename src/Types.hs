{-# LANGUAGE DeriveAnyClass #-}
module Types (
    module Types, module TypesInternal
) where

import GHC.Generics
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.FromField as PSF
import qualified Database.PostgreSQL.Simple.FromRow as PSR
import qualified Database.PostgreSQL.Simple.Types as PST
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified GenericPretty as GP
import qualified Data.Aeson as Ae
import General
import Types.Authors as TypesInternal
import Types.Category as TypesInternal
--import Types.Comments
import Types.Common as TypesInternal
import Types.Draft as TypesInternal
import Types.Posts as TypesInternal
import Types.Tags as TypesInternal
import Types.Users as TypesInternal
import Types.APIErrors as TypesInternal
import Types.Entity as TypesInternal
import Types.APIResult as TypesInternal
{-
-}


data WithUser a = WithUser {
    _wu_userId :: User,
    _wu_action :: a
    } deriving (Show, Generic)

data WithAuthor a = WithAuthor {
    _wa_authorId :: Int,
    _wa_action :: a
    } deriving (Show, Generic)

type Token = T.Text

data WhoWhat a = WhoWhat {
    _ww_token  :: Maybe Token,
    _ww_action :: a
    } deriving (Show, Generic) 








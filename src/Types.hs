module Types (
    module Types, module TypesInternal
) where

import GHC.Generics
import qualified Data.Text as T
import Types.Authors as TypesInternal
import Types.Category as TypesInternal
import Types.Common as TypesInternal
import Types.Draft as TypesInternal
import Types.Posts as TypesInternal
import Types.Tags as TypesInternal
import Types.Users as TypesInternal
import Types.APIErrors as TypesInternal
import Types.Entity as TypesInternal
import Types.APIResult as TypesInternal


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
    } deriving (Show, Eq, Generic) 








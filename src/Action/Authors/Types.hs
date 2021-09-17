module Action.Authors.Types where

import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS
import Action.Common
import Data.Void

--type Author = T.Text

type ActionAuthors = CRUD Void GetAuthors Void Void


data GetAuthors = GetAuthors
    deriving (Show, Generic)


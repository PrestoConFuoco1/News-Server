module Action.Authors.Types where

import GHC.Generics
import qualified GenericPretty as GP
import qualified Database.PostgreSQL.Simple as PS


--type Author = T.Text

data ActionAuthors = AGet GetAuthors


data GetAuthors = GetAuthors
    deriving (Show, Generic)


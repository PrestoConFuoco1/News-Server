{-# LANGUAGE TypeFamilies,
             FlexibleContexts #-}
module Database.HasTags where

import qualified Data.Text as T (pack, Text)
import qualified Database.PostgreSQL.Simple as PS

class (Show (HIdent s)) => HasTags s where
    type HIdent s :: *
    hToInt :: s -> HIdent s -> Int
    hName :: s -> PS.Query
    hName' :: s -> T.Text

newtype HDraft = HDraft ()
dummyHDraft = HDraft ()

instance HasTags HDraft where
    type HIdent HDraft = Int
    hToInt _ = id
    hName _ = "draft"
    hName' _ = "draft"

newtype HPost = HPost ()
dummyHPost = HPost ()

instance HasTags HPost where
    type HIdent HPost = Int
    hToInt _ = id
    hName _ = "post"
    hName' _ = "post"



{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Database.HasTags where

import qualified Data.Text as T (Text)
import qualified Database.PostgreSQL.Simple as PS

class (Show (HIdent s)) =>
      HasTags s
   where
   type HIdent s :: *
   hToInt :: s -> HIdent s -> Int
   hName :: s -> PS.Query
   hName' :: s -> T.Text


data HDraft = HDraft

instance HasTags HDraft where
   type HIdent HDraft = Int
   hToInt _ = id
   hName _ = "draft"
   hName' _ = "draft"


data HPost = HPost

instance HasTags HPost where
   type HIdent HPost = Int
   hToInt _ = id
   hName _ = "post"
   hName' _ = "post"

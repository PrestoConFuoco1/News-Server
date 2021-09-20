module Profiling (withTimePrint) where

import MonadTypes
import Data.Time.Clock

withTimePrint :: (MonadServer m, Show a) => m a -> m a
withTimePrint m = do
    start <- getCurrentTimeS
    x <- m
    stop <- getCurrentTimeS
    printS (diffUTCTime stop start)
    return x

   


module Utils where

import qualified Data.Text as T

showText :: (Show a) => a -> T.Text
showText = T.pack . show

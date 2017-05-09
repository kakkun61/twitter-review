module Model.Parameter.Status (StatusParam(..)) where

import ClassyPrelude
import Model
import Data.Int
import Data.Convertible

data StatusParam = POpen | PTweeted | PClosed
    deriving (Eq, Show, Ord)

instance Param StatusParam where
    toParam POpen    = "open"
    toParam PTweeted = "tweeted"
    toParam PClosed  = "closed"

    fromParam "open"    = Just POpen
    fromParam "tweeted" = Just PTweeted
    fromParam "closed"  = Just PClosed
    fromParam _         = Nothing

instance Convertible StatusParam Int8 where
    safeConvert POpen    = Right 0
    safeConvert PTweeted = Right 1
    safeConvert PClosed  = Right 2

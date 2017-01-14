module Model.Permission
    ( Permission (..)
    ) where

import ClassyPrelude.Yesod
import Data.Convertible

data Permission = Admin | ReadWrite
                  deriving (Show, Read, Eq)

admin     :: Int32
readWrite :: Int32
admin     = 0
readWrite = 1

instance Convertible Permission Int32 where
    safeConvert Admin     = Right 0
    safeConvert ReadWrite = Right 1

instance Convertible Int32 Permission where
    safeConvert n | n == admin     = Right Admin
                  | n == readWrite = Right ReadWrite
                  | otherwise      = convError ("must be " ++ (show admin) ++ " or " ++ (show readWrite)) n

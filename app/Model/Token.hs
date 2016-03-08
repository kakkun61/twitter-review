{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-binds #-}

module Model.Token () where

import ClassyPrelude.Yesod
import Yesod.Auth.GoogleEmail2 (Token)
import qualified Yesod.Auth.GoogleEmail2 as GE2

import GHC.Read

data Token' = Token { accessToken :: Text
                    , tokenType :: Text
                    }
              deriving (Show, Read)

instance Read Token where
    readPrec = (\(Token a t) -> GE2.Token a t) <$> readPrec

derivePersistField "Token"

module Model.GoogleOAuthToken where

import ClassyPrelude.Yesod
import qualified Yesod.Auth.GoogleEmail2 as GE2

data Token = Token { accessToken :: Text
                   , tokenType :: Text
                   }
             deriving (Show, Read, Eq)
derivePersistField "Token"

tokenToGe2Token :: Token -> GE2.Token
tokenToGe2Token (Token a t) = GE2.Token a t

ge2TokenToToken :: GE2.Token -> Token
ge2TokenToToken (GE2.Token a t) = Token a t

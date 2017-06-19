module Slack
    ( post
    ) where

import Prelude
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as W
import Data.Monoid

post :: String -> String -> IO ()
post uri message = do
    void $ W.post uri ["payload" := ("{\"text\":\"" <> (esc message) <> "\"}")]

esc :: String -> String
esc =
    join .map (
        \case
            '\"' -> "\\\""
            c -> [c]
    )

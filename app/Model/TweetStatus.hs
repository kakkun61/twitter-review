module Model.TweetStatus where

import ClassyPrelude.Yesod
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Data.Int
import Model

data TweetStatus = Open
                 | Tweeted { tweetUri :: Text }
                 | Closed
                 deriving (Show, Read, Eq)
derivePersistField "TweetStatus"

instance ToMarkup TweetStatus where
    toMarkup Open = string "Open"
    toMarkup (Tweeted uri) = H.a ! H.href (H.textValue uri) $ string "Tweeted"
    toMarkup Closed = string "Closed"

instance ToTableValue TweetStatus Int8 where
    toTableValue Open        = 0
    toTableValue (Tweeted _) = 1
    toTableValue Closed      = 2

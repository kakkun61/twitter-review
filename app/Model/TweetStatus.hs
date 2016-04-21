module Model.TweetStatus where

import ClassyPrelude.Yesod
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H

data TweetStatus = Open
                 | Tweeted { tweetUri :: Text }
                 | Closed
                 deriving (Show, Read, Eq)
derivePersistField "TweetStatus"

instance ToMarkup TweetStatus where
    toMarkup Open = string "Open"
    toMarkup (Tweeted uri) = H.a ! H.href (H.textValue uri) $ string "Tweeted"
    toMarkup Closed = string "Closed"

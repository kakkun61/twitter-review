module Model.TweetStatus where

import ClassyPrelude.Yesod
import Text.Blaze

data TweetStatus = Open
                 | Tweeted { tweetUri :: Text }
                 | Closed
                 deriving (Show, Read, Eq)
derivePersistField "TweetStatus"

instance ToMarkup TweetStatus where
    toMarkup = string . show

module Model.TweetStatus where

import ClassyPrelude.Yesod

data TweetStatus = Open
                 | Tweeted { tweetUri :: Text }
                 | Closed
                 deriving (Show, Read, Eq)
derivePersistField "TweetStatus"

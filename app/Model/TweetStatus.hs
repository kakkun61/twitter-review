module Model.TweetStatus where

import ClassyPrelude.Yesod
-- import Database.Persist.Quasi

data TweetStatus = Open
                 | Tweeted { tweetUri :: Text }
                 | Closed
                 deriving (Show, Read, Eq)
derivePersistField "TweetStatus"

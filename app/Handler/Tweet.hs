module Handler.Tweet
 ( getTweetR
 , postTweetR
 ) where

import Import
import Database.Persist.Sql

getTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
getTweetR accountIdParam tweetIdParam = runDB $ do
    account <- entityVal <$> getBy404 (UniqueAccount accountIdParam)
    tweet <- get404 $ toSqlKey tweetIdParam
    lift $ defaultLayout $(widgetFile "tweet")

postTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
postTweetR accountIdParam tweetIdParam = error "not yet implemented"

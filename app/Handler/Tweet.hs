module Handler.Tweet
 ( getTweetR
 , postTweetR
 ) where

import Import

getTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
getTweetR accountIdParam tweetIdParam = runDB $ do
    user <- lift requireAuth
    account <- entityVal <$> getBy404 (UniqueAccount accountIdParam)
    tweet <- get404 $ toSqlKey tweetIdParam
    tweetUser <- get404 $ tweetUserId tweet
    lift $ defaultLayout $ do
        headerWidget $ Just $ entityVal user
        $(widgetFile "tweet")

postTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
postTweetR accountIdParam tweetIdParam = error "not yet implemented"

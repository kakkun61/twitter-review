module Handler.Tweet
 ( getTweetR
 , postTweetR
 ) where

import Import
import qualified Model.Table.Account as Account
import qualified Model.Table.Tweet as Tweet
import qualified Model.Table.User as User

getTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
getTweetR accountIdParam tweetIdParam = runRelational $ do
    user <- lift requireAuth
    accounts <- runQuery Account.selectAccount accountIdParam
    case accounts of
        [account@(Account _ _ _ _)] -> do
            tweets <- runQuery Tweet.selectTweet tweetIdParam
            case tweets of
                [tweet@(Tweet _ _ tweetUserId _ _)] -> do
                    tweetUsers <- runQuery User.selectUser tweetUserId
                    case tweetUsers of
                        [tweetUser@(User _ _ _ _)] -> do
                            lift $ defaultLayout $ do
                                headerWidget $ Just user
                                $(widgetFile "tweet")
                        _ -> lift notFound
                _ -> lift notFound
        _ -> lift notFound

postTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
postTweetR _accountIdParam _tweetIdParam = error "not yet implemented"

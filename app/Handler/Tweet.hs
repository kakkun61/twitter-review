module Handler.Tweet
 ( getTweetR
 , postTweetR
 ) where

import Import
import Model.Table.Account (Account(..))
import qualified Model.Table.Account as Account
import Model.Table.Tweet (Tweet(..))
import qualified Model.Table.Tweet as Tweet
import Model.Table.User (User(..))
import qualified Model.Table.User as User

getTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
getTweetR accountIdParam tweetIdParam = runRelational $ do
    user <- lift requireAuth
    accounts <- flip runQuery () $ relationalQuery $ relation $ do
                    a <- query Account.account
                    wheres $ a ! Account.id' .=. value accountIdParam
                    return a
    case accounts of
        [account@(Account _ _ _ _)] -> do
            tweets <- flip runQuery () $ relationalQuery $ relation $ do
                          t <- query Tweet.tweet
                          wheres $ t ! Tweet.id' .=. value tweetIdParam
                          return t
            case tweets of
                [tweet@(Tweet _ _ tweetUserId _ _)] -> do
                    tweetUsers <- flip runQuery () $ relationalQuery $ relation $ do
                                     u <- query User.user
                                     wheres $ u ! User.id' .=. value tweetUserId
                                     return u
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

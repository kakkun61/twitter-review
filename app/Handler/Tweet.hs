module Handler.Tweet
 ( getTweetR
 , postTweetR
 ) where

import Import hiding (on)
import Database.Relational.Query (on)
import qualified Model.Table.Account as Account
import qualified Model.Table.Tweet as Tweet
import qualified Model.Table.User as User

getTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
getTweetR accountIdParam tweetIdParam = do
    user <- requireAuth
    p <- runRelational $ do
             accounts <- runQuery' Account.selectAccount accountIdParam
             ts <- flip runQuery' () $ relationalQuery $ relation $ do
                       t <- query Tweet.tweet
                       u <- query User.user
                       on $ t ! Tweet.userId' .=. u ! User.id'
                       wheres $ t ! Tweet.id' .=. value tweetIdParam
                       return $ (,) |$| t |*| u
             return (accounts, ts)
    case p of
        ([account@(Account _ _ _ _)], [(tweet@(Tweet _ _ _ _ _), tweetUser@(User _ _ _ _))]) -> do
            form <- generateFormPost commentForm
            defaultLayout $ do
                headerWidget $ Just user
                tweetWidget account user tweet form
        _ -> notFound

postTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
postTweetR _accountIdParam _tweetIdParam = error "not yet implemented"


newtype TweetFormData = TweetFormData { tweetFormText :: Text }
    deriving Show

tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

newtype CommentFormData = CommentFormData { commentFormText :: Text }
    deriving Show

commentForm :: Html -> MForm Handler (FormResult CommentFormData, Widget)
commentForm = renderDivs $ CommentFormData <$> areq textField "Comment" Nothing

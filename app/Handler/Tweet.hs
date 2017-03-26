module Handler.Tweet
 ( getTweetR
 , postTweetR
 ) where

import Import hiding (on)
import Data.Time.LocalTime
import Database.HDBC (commit)
import Database.Relational.Query (on)
import qualified Model.Table.Account as Account
import qualified Model.Table.Comment as Comment
import qualified Model.Table.Tweet as Tweet
import qualified Model.Table.User as User

getTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
getTweetR = tweetR GET

postTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
postTweetR = tweetR POST

tweetR :: StdMethod -> AccountIdParam -> TweetIdParam -> Handler Html
tweetR method accountIdParam tweetIdParam = do
    user <- requireAuth
    nowUtc <- liftIO getCurrentTime
    let nowLt = utcToLocalTime utc nowUtc
    p <- runRelational $ do
             accounts <- runQuery' Account.selectAccount accountIdParam
             ts <- flip runQuery' () $ relationalQuery $ relation $ do
                       t <- query Tweet.tweet
                       u <- query User.user
                       on $ t ! Tweet.userId' .=. u ! User.id'
                       wheres $ t ! Tweet.id' .=. value tweetIdParam
                       return $ (,) |$| t |*| u
             comments <- case ts of
                           [(tweet, _)] ->
                               flip runQuery' () $ relationalQuery $ relation $ do
                                   c <- query Comment.comment
                                   wheres $ c ! Comment.tweetId' .=. value (Tweet.id tweet)
                                   return c
                           _ -> return []
             return (accounts, ts, comments)
    case p of
        ([account], [(tweet, tweetUser)], comments) -> do
            when (method == POST) $ treatPostedForm tweet user nowLt
            form <- generateFormPost commentForm
            defaultLayout $ do
                headerWidget $ Just user
                tweetWidget account tweetUser tweet comments form
        _ -> notFound
    where
        treatPostedForm :: Tweet -> User -> LocalTime -> Handler ()
        treatPostedForm tweet user now = do
            ((result, widget), enctype) <- runFormPost commentForm
            case result of
                FormSuccess commentFormData -> do
                    void $ runRelational $ do
                        void $ runInsert Comment.insertCommentNoId (CommentNoId (Tweet.id tweet) (convert $ commentFormText commentFormData) (User.id user) now)
                        run commit
                FormFailure err -> do
                    $(logDebug) $ unlines err
                    return ()
                FormMissing -> do
                    return ()

newtype TweetFormData = TweetFormData { tweetFormText :: Text }
    deriving Show

tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

newtype CommentFormData = CommentFormData { commentFormText :: Text }
    deriving Show

commentForm :: Html -> MForm Handler (FormResult CommentFormData, Widget)
commentForm = renderDivs $ CommentFormData <$> areq textField "Comment" Nothing

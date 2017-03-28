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
import qualified Model.Table.TweetCandidate as TweetCandidate
import qualified Model.Table.User as User

getTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
getTweetR = tweetR GET

postTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
postTweetR = tweetR POST

tweetR :: StdMethod -> AccountIdParam -> TweetIdParam -> Handler Html
tweetR method accountIdParam tweetIdParam = runRelational $ do
    user <- lift requireAuth
    nowUtc <- liftIO getCurrentTime
    let nowLt = utcToLocalTime utc nowUtc
    accounts <- runQuery' Account.selectAccount accountIdParam
    ts <- flip runQuery' () $ relationalQuery $ relation $ do
              t <- query Tweet.tweet
              u <- query User.user
              on $ t ! Tweet.userId' .=. u ! User.id'
              wheres $ t ! Tweet.id' .=. value tweetIdParam
              return $ (,) |$| t |*| u
    case (accounts, ts) of
        ([account], [(tweet, tweetUser)]) -> do
            commentWE <- case method of
                             POST -> treatPostedCommentForm tweet user nowLt
                             _ -> lift $ generateFormPost commentForm
            tweetWE <- case method of
                           POST -> treatPostedTweetForm tweet user nowLt
                           _ -> lift $ generateFormPost tweetForm
            comments <- flip runQuery' () $ relationalQuery $ relation $ do
                            c <- query Comment.comment
                            wheres $ c ! Comment.tweetId' .=. value (Tweet.id tweet)
                            return c
            lift $ defaultLayout $ do
                headerWidget $ Just user
                tweetWidget account tweetUser tweet comments tweetWE commentWE
        _ -> lift $ notFound
    where
        treatPostedCommentForm :: Tweet -> User -> LocalTime -> YesodRelationalMonad App (Widget, Enctype)
        treatPostedCommentForm tweet user now = do
            ((result, widget), enctype) <- lift $ runFormPost commentForm
            case result of
                FormSuccess commentFormData -> do
                    void $ runInsert Comment.insertCommentNoId (CommentNoId (Tweet.id tweet) (convert $ commentFormText commentFormData) (User.id user) now)
                    run commit
                FormFailure err -> do
                    $(logDebug) $ unlines err
                    return ()
                FormMissing -> do
                    return ()
            return (widget, enctype)
        treatPostedTweetForm :: Tweet -> User -> LocalTime -> YesodRelationalMonad App (Widget, Enctype)
        treatPostedTweetForm tweet user now = do
            ((result, widget), enctype) <- lift $ runFormPost tweetForm
            case result of
                FormSuccess tweetFormData -> do
                    void $ runInsert TweetCandidate.insertTweetCandidateNoId (TweetCandidateNoId (Tweet.id tweet) (convert $ tweetFormText tweetFormData) (User.id user) now)
                    run commit
                FormFailure err -> do
                    $(logDebug) $ unlines err
                    return ()
                FormMissing -> return ()
            return (widget, enctype)

newtype TweetFormData = TweetFormData { tweetFormText :: Text }
    deriving Show

tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
tweetForm = identifyForm "tweet" $ renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

newtype CommentFormData = CommentFormData { commentFormText :: Text }
    deriving Show

commentForm :: Html -> MForm Handler (FormResult CommentFormData, Widget)
commentForm = identifyForm "comment" $ renderDivs $ CommentFormData <$> areq textField "Comment" Nothing

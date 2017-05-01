module Handler.Tweet
 ( getTweetR
 , postTweetR
 ) where

import Import hiding (on)
import Data.Time.LocalTime
import qualified Data.ByteString.Char8 as BS
import Database.HDBC (commit)
import Database.Relational.Query (on)
import qualified Web.Authenticate.OAuth as OAuth
import qualified Web.Twitter.Conduit as Twitter
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
            candidateWE <- case method of
                               POST -> treatPostedCandidateForm tweet user nowLt
                               _ -> lift $ generateFormPost candidateForm
            comments <- flip runQuery' () $ relationalQuery $ relation $ do
                            c <- query Comment.comment
                            u <- query User.user
                            wheres $ c ! Comment.tweetId' .=. value (Tweet.id tweet)
                            on $ c ! Comment.userId' .=. u ! User.id'
                            return $ (,) |$| c |*| u
            candidates <- flip runQuery' () $ relationalQuery $ relation $ do
                              c <- query TweetCandidate.tweetCandidate
                              u <- query User.user
                              wheres $ c ! TweetCandidate.tweetId' .=. value (Tweet.id tweet)
                              on $ c ! TweetCandidate.userId' .=. u ! User.id'
                              return $ (,) |$| c |*| u
            let ccs = flip sortBy (mix comments candidates) $ \a b ->
                          let [aTime, bTime] = fmap (either (Comment.created . fst) (TweetCandidate.created . fst)) [a, b]
                          in compare aTime bTime
            (tweetWE, tweet') <- case method of
                           POST -> case lastMay candidates of
                                       Just (candidate, _) -> treatPostedTweetForm account tweet candidate
                                       Nothing -> lift $ (, tweet) <$> generateFormPost tweetForm
                           _ -> lift $ (, tweet) <$> generateFormPost tweetForm
            lift $ defaultLayout $ do
                headerWidget $ Just user
                tweetWidget account tweetUser tweet' ccs candidateWE commentWE tweetWE
        _ -> lift $ notFound

    where
        treatPostedCommentForm :: Tweet -> User -> LocalTime -> YesodRelationalMonad App (Widget, Enctype)
        treatPostedCommentForm tweet user now = do
            ((result, widget), enctype) <- lift $ runFormPost commentForm
            logFormResult "comment" result
            case result of
                FormSuccess commentFormData -> do
                    void $ runInsert Comment.insertCommentNoId (CommentNoId (Tweet.id tweet) (convert $ commentFormText commentFormData) (User.id user) now)
                    run commit
                FormFailure err -> do
                    $(logWarn) $ unlines err
                    return ()
                FormMissing -> do
                    return ()
            return (widget, enctype)

        treatPostedCandidateForm :: Tweet -> User -> LocalTime -> YesodRelationalMonad App (Widget, Enctype)
        treatPostedCandidateForm tweet user now = do
            ((result, widget), enctype) <- lift $ runFormPost candidateForm
            logFormResult "candidate" result
            case result of
                FormSuccess candidateFormData -> do
                    void $ runInsert TweetCandidate.insertTweetCandidateNoId (TweetCandidateNoId (Tweet.id tweet) (convert $ candidateFormText candidateFormData) (User.id user) now)
                    run commit
                FormFailure err -> do
                    $(logDebug) $ unlines err
                    return ()
                FormMissing -> return ()
            return (widget, enctype)

        treatPostedTweetForm :: Account -> Tweet -> TweetCandidate -> YesodRelationalMonad App ((Widget, Enctype), Tweet)
        treatPostedTweetForm account tweet candidate = do
            ((result, widget), enctype) <- lift $ runFormPost tweetForm
            logFormResult "tweet" result
--             case result of
--                 FormSuccess _ -> do
--                     master <- lift $ getYesod
--                     let httpManager = getHttpManager master
--                     let credential = OAuth.Credential [ ("oauth_token", BS.pack (Account.token account))
--                                                       , ("oauth_token_secret", BS.pack (Account.tokenSecret account))
--                                                       ]
--                     let token = def { Twitter.twOAuth = mkTwitterOAuth master
--                                     , Twitter.twCredential = credential
--                                     }
--                     let twInfo = def { Twitter.twToken = token }
--                     status <- Twitter.call twInfo httpManager $ Twitter.update $ pack $ TweetCandidate.text candidate
--                     $(logDebug) $ "status: " <> (pack $ show status)
--                     return ()
--                 FormFailure err -> do
--                     $(logDebug) $ unlines err
--                     return ()
--                 FormMissing -> return ()
            return ((widget, enctype), tweet)

        mix :: (Functor f, Semigroup (f (Either a b))) => f a -> f b -> f (Either a b)
        mix l r = fmap Left l <> fmap Right r

newtype CandidateFormData = CandidateFormData { candidateFormText :: Text }
    deriving Show

candidateForm :: Html -> MForm Handler (FormResult CandidateFormData, Widget)
candidateForm = identifyForm "candidate" $ renderDivs $ CandidateFormData <$> areq textField "Candidate" Nothing

newtype CommentFormData = CommentFormData { commentFormText :: Text }
    deriving Show

commentForm :: Html -> MForm Handler (FormResult CommentFormData, Widget)
commentForm = identifyForm "comment" $ renderDivs $ CommentFormData <$> areq textField "Comment" Nothing

tweetForm :: Html -> MForm Handler (FormResult (), Widget)
tweetForm = identifyForm "tweet" $ renderDivs $ pure ()

logFormResult tag result = $(logDebug) $ (tag <>) $ case result of
                                                        FormSuccess _ -> " success"
                                                        FormFailure _ -> " failure"
                                                        FormMissing   -> " missing"

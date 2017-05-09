module Handler.Tweet
 ( getTweetR
 , postTweetR
 ) where

import Import hiding (on)
import qualified Import as F (on)
import Data.Time.LocalTime
import qualified Data.ByteString.Char8 as BS
import Database.HDBC (commit)
import Database.Relational.Query (on)
import Control.Lens
import qualified Web.Authenticate.OAuth as OAuth
import qualified Web.Twitter.Conduit as Twitter
import qualified Web.Twitter.Types as Twitter
import qualified Model.Table.Account as Account
import qualified Model.Table.Comment as Comment
import qualified Model.Table.Tweet as Tweet
import qualified Model.Table.TweetCandidate as TweetCandidate
import qualified Model.Table.TweetUri as TweetUri
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
            (closeWE, tweet') <- case method of
                                     POST -> treatPostedCloseForm tweet
                                     _ -> lift $ (, tweet) <$> generateFormPost closeForm
            comments <- flip runQuery' () $ relationalQuery $ relation $ do
                            c <- query Comment.comment
                            u <- query User.user
                            wheres $ c ! Comment.tweetId' .=. value (Tweet.id tweet')
                            on $ c ! Comment.userId' .=. u ! User.id'
                            return $ (,) |$| c |*| u
            candidates <- (sortBy (compare `F.on` (TweetCandidate.created . fst)) <$>) $ flip runQuery' () $ relationalQuery $ relation $ do
                              c <- query TweetCandidate.tweetCandidate
                              u <- query User.user
                              wheres $ c ! TweetCandidate.tweetId' .=. value (Tweet.id tweet')
                              on $ c ! TweetCandidate.userId' .=. u ! User.id'
                              return $ (,) |$| c |*| u
            let ccs = sortBy (compare `F.on` either (Comment.created . fst) (TweetCandidate.created . fst)) (mix comments candidates)
            (tweetWE, tweet'') <- case method of
                           POST -> case lastMay candidates of
                                       Just (candidate, _) -> treatPostedTweetForm account tweet' candidate
                                       Nothing -> lift $ (, tweet') <$> generateFormPost tweetForm
                           _ -> lift $ (, tweet') <$> generateFormPost tweetForm
            tweetUri <- runQuery' TweetUri.selectTweetUri (Tweet.id tweet'') >>= \case
                            [uri] -> return $ Just $ pack $ TweetUri.uri uri
                            _ -> return Nothing
            lift $ defaultLayout $ do
                headerWidget $ Just user
                tweetWidget account tweetUser tweet'' tweetUri ccs candidateWE commentWE tweetWE closeWE
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
                    $(logWarn) $ unlines err
                    return ()
                FormMissing -> do
                    return ()
            return (widget, enctype)

        treatPostedCandidateForm :: Tweet -> User -> LocalTime -> YesodRelationalMonad App (Widget, Enctype)
        treatPostedCandidateForm tweet user now = do
            ((result, widget), enctype) <- lift $ runFormPost candidateForm
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
            case result of
                FormSuccess _ -> do
                    master <- lift $ getYesod
                    let httpManager = getHttpManager master
                    let credential = OAuth.Credential [ ("oauth_token", BS.pack (Account.token account))
                                                      , ("oauth_token_secret", BS.pack (Account.tokenSecret account))
                                                      ]
                    let token = def { Twitter.twOAuth = mkTwitterOAuth master
                                    , Twitter.twCredential = credential
                                    }
                    let twInfo = def { Twitter.twToken = token }
                    do
                        status <- liftIO $ Twitter.call twInfo httpManager $ Twitter.update (pack $ TweetCandidate.text candidate) -- & Twitter.trimUser ?~ True -- trimUser すると返ってくる JSON が型に合わない
                        void $ runInsert TweetUri.insertTweetUri (TweetUri.TweetUri (Tweet.id tweet) ("https://twitter.com/" <> (Account.screenName account) <> "/status/" <> (show $ Twitter.statusId status)))
                        void $ runUpdate (updateTweetStatus (Tweet.id tweet) (Tweeted "")) ()
                        run commit
                        return ()
                    `catch`
                    \(e :: SomeException) -> do
                        setMessage "failed to tweet"
                        $(logDebug) $ pack $ show e
                FormFailure err -> do
                    $(logDebug) $ unlines err
                    return ()
                FormMissing -> return ()
            return ((widget, enctype), tweet)

        treatPostedCloseForm :: Tweet -> YesodRelationalMonad App ((Widget, Enctype), Tweet)
        treatPostedCloseForm tweet = do
            ((result, widget), enctype) <- lift $ runFormPost closeForm
            tweet' <- case result of
                          FormSuccess candidateFormData -> do
                              void $ runUpdate (updateTweetStatus (Tweet.id tweet) (Closed)) ()
                              run commit
                              return $ tweet { Tweet.status = convert Closed }
                          FormFailure err -> do
                              $(logDebug) $ unlines err
                              return tweet
                          FormMissing -> return tweet
            return ((widget, enctype), tweet')


        updateTweetStatus :: Int64 -> TweetStatus -> Update ()
        updateTweetStatus tid status =
            typedUpdate Tweet.tableOfTweet . updateTarget $ \proj -> do
                Tweet.status' <-# value (convert status)
                wheres $ proj ! Tweet.id' .=. value tid

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

closeForm :: Html -> MForm Handler (FormResult (), Widget)
closeForm = identifyForm "close" $ renderDivs $ pure ()

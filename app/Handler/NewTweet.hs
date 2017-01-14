module Handler.NewTweet where

import Import
import Data.Time.LocalTime
import Database.HDBC (commit)
import qualified Model.Table.User as User
import Model.Table.Account (Account(..))
import qualified Model.Table.Account as Account
import Model.Table.Tweet (TweetNoId(..))
import qualified Model.Table.Tweet as Tweet
import Model.Table.TweetCandidate (TweetCandidateNoId(..))
import qualified Model.Table.TweetCandidate as TweetCandidate

getNewTweetR :: AccountIdParam -> Handler Html
getNewTweetR accountIdParam = do
    user <- requireAuth
    (widget, enctype) <- generateFormPost tweetForm
    defaultLayout $ do
        headerWidget $ Just user
        $(widgetFile "new-tweet")

postNewTweetR :: AccountIdParam -> Handler Html
postNewTweetR accountIdParam = do
    user <- requireAuth
    nowUtc <- lift getCurrentTime
    let nowLt = utcToLocalTime utc nowUtc
    ((result, widget), enctype) <- runFormPost tweetForm
    case result of
        FormSuccess tweetFormData -> do
            let text = tweetFormText tweetFormData
            tweetId <- runRelational $ do
                accounts <- flip runQuery () $ relationalQuery $ relation $ do
                                a <- query Account.account
                                wheres $ a ! Account.id' .=. value accountIdParam
                                return a
                case accounts of
                    [Account accountId _ _ _] -> do
                        tweetId <- fromIntegral <$> runInsert Tweet.insertTweetNoId (TweetNoId accountId (User.id user) (show Open) nowLt)
                        void $ runInsert TweetCandidate.insertTweetCandidateNoId $ TweetCandidateNoId tweetId (show text) (User.id user) nowLt
                        run commit
                        return tweetId
                    _ -> error "unexpected"
            redirect $ TweetR accountIdParam tweetId
        FormFailure err -> defaultLayout $ do
            $(logDebug) $ unlines err
            headerWidget $ Just user
            $(widgetFile "new-tweet")
        FormMissing -> defaultLayout $ do
            headerWidget $ Just user
            $(widgetFile "new-tweet")

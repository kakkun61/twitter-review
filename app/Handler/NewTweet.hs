module Handler.NewTweet where

import Import
import Database.Persist.Sql

getNewTweetR :: AccountIdParam -> Handler Html
getNewTweetR accountIdParam = do
    user <- requireAuth
    (widget, enctype) <- generateFormPost tweetForm
    defaultLayout $ do
        headerWidget $ Just $ entityVal user
        $(widgetFile "new-tweet")

postNewTweetR :: AccountIdParam -> Handler Html
postNewTweetR accountIdParam = do
    user <- requireAuth
    now <- lift getCurrentTime
    ((result, widget), enctype) <- runFormPost tweetForm
    case result of
        FormSuccess tweetFormData -> do
            let text = tweetFormText tweetFormData
            runDB $ do
                account <- getBy404 $ UniqueAccount accountIdParam
                tweetId <- insert $ Tweet text (entityKey account) (entityKey user) Open now
                lift $ redirect $ TweetR accountIdParam (fromSqlKey tweetId)
        FormFailure err -> defaultLayout $ do
            $(logDebug) $ unlines err
            headerWidget $ Just $ entityVal user
            $(widgetFile "new-tweet")
        FormMissing -> defaultLayout $ do
            headerWidget $ Just $ entityVal user
            $(widgetFile "new-tweet")

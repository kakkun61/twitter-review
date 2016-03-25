module Handler.Tweet where

import Import
import Database.Persist.Sql

getTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
getTweetR accountIdParam tweetIdParam = do
    (widget, enctype) <- generateFormPost tweetForm
    let retry = False
    defaultLayout $(widgetFile "tweet")

postTweetR :: AccountIdParam -> TweetIdParam -> Handler Html
postTweetR accountIdParam tweetIdParam = do
    user <- requireAuth
    now <- lift getCurrentTime
    ((result, widget), enctype) <- runFormPost tweetForm
    case result of
        FormSuccess tweetFormData -> do
            let text = tweetFormText tweetFormData
            _ <- runDB $ insert $ Tweet text (toSqlKey 1) (toSqlKey 1) Open now
            defaultLayout [whamlet|
                              <p>account Id param: #{accountIdParam}
                              <p>status id: #{tweetIdParam}
                              <p>tweet text: #{text}
                          |]
        _ -> do
            let retry = True
            defaultLayout $(widgetFile "tweet")

newtype TweetFormData = TweetFormData { tweetFormText :: Text }
    deriving Show

tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

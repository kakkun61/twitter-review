module Handler.Tweet where

import Import
import Database.Persist.Sql

getTweetR :: ScreenName -> TweetId -> Handler Html
getTweetR screenName statusId = do
    (widget, enctype) <- generateFormPost tweetForm
    let retry = False
    defaultLayout $(widgetFile "tweet")

postTweetR :: ScreenName -> TweetId -> Handler Html
postTweetR screenName statusId = do
    now <- lift getCurrentTime
    ((result, widget), enctype) <- runFormPost tweetForm
    case result of
        FormSuccess tweetFormData -> do
            let text = tweetFormText tweetFormData
            _ <- runDB $ insert $ Tweet text (toSqlKey 1) Open now
            defaultLayout [whamlet|
                              <p>screen name: #{screenName}
                              <p>status id: #{fromSqlKey statusId}
                              <p>tweet text: #{text}
                          |]
        _ -> do
            let retry = True
            defaultLayout $(widgetFile "tweet")

newtype TweetFormData = TweetFormData { tweetFormText :: Text }
    deriving Show

tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

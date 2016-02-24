module Handler.Tweet where

import Import
import qualified Data.Set as S
import Database.Persist.Sql

getTweetR :: ScreenName -> TweetId -> Handler Html
getTweetR screenName statusId = do-- error "Not yet implemented: getTweetR"
    defaultLayout $(widgetFile "tweet")

postTweetR :: ScreenName -> TweetId -> Handler Html
postTweetR screenName statusId = do
    johnId <- runDB $ insert $ User "john" Nothing
--     let tweet = Tweet "Hi" johnId (S.fromList [])
    defaultLayout [whamlet|
                      <p>user id: #{fromSqlKey johnId}
                  |]

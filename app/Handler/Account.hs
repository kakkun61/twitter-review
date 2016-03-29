module Handler.Account where

import Import

getAccountR :: AccountIdParam -> Handler Html
getAccountR accountIdParam = do
    user <- entityVal <$> requireAuth
    runDB $ do
        accountEntity <- getBy404 (UniqueAccount accountIdParam)
        let account = entityVal accountEntity
        tweetEntities <- selectList [TweetAccountId ==. entityKey accountEntity] [Desc TweetCreated]
        lift $ defaultLayout $ do
            headerWidget $ Just user
            $(widgetFile "account")

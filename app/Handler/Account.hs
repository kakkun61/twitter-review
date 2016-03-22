module Handler.Account where

import Import

getAccountR :: AccountIdParam -> Handler Html
getAccountR accountIdParam = do
    user <- entityVal <$> requireAuth
    runDB $ do
        account <- entityVal <$> getBy404 (UniqueAccount accountIdParam)
        lift $ defaultLayout $ do
            headerWidget $ Just user
            $(widgetFile "account")

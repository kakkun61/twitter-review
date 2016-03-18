module Handler.Account where

import Import

getAccountR :: ScreenName -> Handler Html
getAccountR screenName = do
    user <- entityVal <$> requireAuth
    defaultLayout $ do
        let mUser = Just user
        $(widgetFile "header")
        $(widgetFile "account")

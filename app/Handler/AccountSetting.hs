module Handler.AccountSetting where

import Import

getAccountSettingR :: ScreenName -> Handler Html
getAccountSettingR screenName = do
    user <- entityVal <$> requireAuth
    defaultLayout $ do
        headerWidget $ Just user
        $(widgetFile "account-setting")

postAccountSettingR :: ScreenName -> Handler Html
postAccountSettingR screenName = error "Not yet implemented: postAccountSettingR"

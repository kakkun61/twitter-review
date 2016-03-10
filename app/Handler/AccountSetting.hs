module Handler.AccountSetting where

import Import

getAccountSettingR :: ScreenName -> Handler Html
getAccountSettingR screenName = do
    mUser <- fmap (fmap entityVal) maybeAuth
    let header = $(widgetFile "header")
    defaultLayout $(widgetFile "account-setting")

postAccountSettingR :: ScreenName -> Handler Html
postAccountSettingR screenName = error "Not yet implemented: postAccountSettingR"

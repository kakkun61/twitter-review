module Handler.AccountSetting where

import Import

getAccountSettingR :: ScreenName -> Handler Html
getAccountSettingR screenName = do
    defaultLayout [whamlet|
                      <p>@#{screenName} Setting
                  |]

postAccountSettingR :: ScreenName -> Handler Html
postAccountSettingR screenName = error "Not yet implemented: postAccountSettingR"

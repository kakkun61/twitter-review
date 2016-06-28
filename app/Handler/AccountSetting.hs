module Handler.AccountSetting where

import Import

getAccountSettingR :: AccountIdParam -> Handler Html
getAccountSettingR = error "don't call me temporally"
-- getAccountSettingR accountIdParam = do
--     user <- entityVal <$> requireAuth
--     runDB $ do
--         account <- entityVal <$> getBy404 (UniqueAccount accountIdParam)
--         lift $ defaultLayout $ do
--             headerWidget $ Just user
--             $(widgetFile "account-setting")

postAccountSettingR :: AccountIdParam -> Handler Html
postAccountSettingR _screenName = error "Not yet implemented: postAccountSettingR"

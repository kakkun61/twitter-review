module Handler.AccountSetting where

import Import

getAccountSettingR :: AccountIdParam -> Handler Html
getAccountSettingR = undefined
-- getAccountSettingR accountIdParam = do
--     user <- entityVal <$> requireAuth
--     runDB $ do
--         account <- entityVal <$> getBy404 (UniqueAccount accountIdParam)
--         lift $ defaultLayout $ do
--             headerWidget $ Just user
--             $(widgetFile "account-setting")

postAccountSettingR :: AccountIdParam -> Handler Html
postAccountSettingR screenName = error "Not yet implemented: postAccountSettingR"

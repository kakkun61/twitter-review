module Handler.AccountSetting where

import Import
import qualified Model.Table.Account as Account

getAccountSettingR :: AccountIdParam -> Handler Html
getAccountSettingR accountIdParam = do
    user <- requireAuth
    runRelational $ do
        mAccount <- flip runQuery () $ relationalQuery $ relation $ do
                        a <- query Account.account
                        wheres $ a ! Account.id' .=. value accountIdParam
                        return a
        case mAccount of
            [account] -> lift $ defaultLayout $ do
                             headerWidget $ Just user
                             accountSettingWidget account
            [] -> lift notFound
            _ -> error "unexpected"

postAccountSettingR :: AccountIdParam -> Handler Html
postAccountSettingR _screenName = error "Not yet implemented: postAccountSettingR"

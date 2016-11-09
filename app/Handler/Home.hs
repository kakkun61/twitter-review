module Handler.Home where

import Import hiding (Set, on)
import Database.Relational.Query
import qualified Model.Table.User as User
import qualified Model.Table.Account as Account
import qualified Model.Table.UserAccountRelation as UAR

getHomeR :: Handler Html
getHomeR = do
    mUser <- maybeAuth
    $(logDebug) $ pack $ show mUser
    case mUser of
        Just user -> do
            accounts <- runRelational $ flip runQuery () $ relationalQuery $ relation $ do
                a <- query Account.account
                r <- query UAR.userAccountRelation
                on $ a ! Account.id' .=. r ! UAR.accountId'
                wheres $ r ! UAR.userId' .=. value (User.id user)
                return a
            $(logDebug) $ pack $ show accounts
            defaultLayout $ do
                headerWidget mUser
                homeWidget mUser accounts
        Nothing ->
            defaultLayout $ do
                let accounts = []
                headerWidget Nothing
                homeWidget Nothing []

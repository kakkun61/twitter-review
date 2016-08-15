module Handler.Home where

import Import
import qualified Database.Esqueleto as E
import Model.Table.User
import qualified Database.Relational.Query as RR
import qualified Yesod.Relational as YR
import qualified Yesod.Auth.Relational as YR

getHomeR :: Handler Html
getHomeR = do
--     u <- YR.maybeAuth
--     $(logDebug) $ pack $ show u
--     mUser <- maybeAuth
    $(logDebug) $ pack $ "HRR! " ++ show user
    r <- YR.runRelational $ YR.runQuery (RR.relationalQuery $ RR.relation $ RR.query user) ()
    $(logDebug) $ pack $ "HRR! " ++ show r
    case r of
        [user] -> error "under construction"
--             runDB $ do
--                 accounts <- ((entityVal <$>) <$>) $ E.select $ E.from $ \(acc `E.InnerJoin` rel) -> do
--                     E.on $ acc E.^. AccountId E.==. rel E.^. UserAccountRelationAccountId
--                     E.where_ $ rel E.^. UserAccountRelationUserId E.==. E.val (entityKey user)
--                     return acc
--                 lift $ defaultLayout $ do
--                     let signinWithGoogle = $(widgetFile "signin-with-google")
--                     headerWidget $ Just $ entityVal user
--                     $(widgetFile "home")
        [] ->
            defaultLayout $ do
                let signinWithGoogle = $(widgetFile "signin-with-google")
                let accounts = []
                headerWidget Nothing
                homeWidget Nothing
        otherwise -> error "unexpected"

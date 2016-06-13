module Handler.Home where

import Import
import qualified Database.Esqueleto as E
import Model.Table.User
import qualified Database.Relational.Query as RR
import qualified Yesod.Relational as YR

getHomeR :: Handler Html
getHomeR = do
    mUser <- maybeAuth
    $(logDebug) $ pack $ show user
    r <- YR.runRelational $ YR.runQuery (RR.relationalQuery $ RR.relation $ RR.query user) ()
    $(logDebug) $ pack $ show r
    case mUser of
        Just user ->
            runDB $ do
                accounts <- ((entityVal <$>) <$>) $ E.select $ E.from $ \(acc `E.InnerJoin` rel) -> do
                    E.on $ acc E.^. AccountId E.==. rel E.^. UserAccountRelationAccountId
                    E.where_ $ rel E.^. UserAccountRelationUserId E.==. E.val (entityKey user)
                    return acc
                lift $ defaultLayout $ do
                    let signinWithGoogle = $(widgetFile "signin-with-google")
                    headerWidget $ Just $ entityVal user
                    $(widgetFile "home")
        Nothing ->
            defaultLayout $ do
                let signinWithGoogle = $(widgetFile "signin-with-google")
                let accounts = []
                headerWidget $ entityVal <$> mUser
                $(widgetFile "home")

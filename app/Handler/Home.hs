module Handler.Home where

import Import hiding (Set)
import Database.Relational.Query

getHomeR :: Handler Html
getHomeR = do
    mUser <- maybeAuth
    $(logDebug) $ pack $ show mUser
    case mUser of
        Just user -> error "under construction"
--             runDB $ do
--                 accounts <- ((entityVal <$>) <$>) $ E.select $ E.from $ \(acc `E.InnerJoin` rel) -> do
--                     E.on $ acc E.^. AccountId E.==. rel E.^. UserAccountRelationAccountId
--                     E.where_ $ rel E.^. UserAccountRelationUserId E.==. E.val (entityKey user)
--                     return acc
--                 lift $ defaultLayout $ do
--                     let signinWithGoogle = $(widgetFile "signin-with-google")
--                     headerWidget $ Just $ entityVal user
--                     $(widgetFile "home")
        Nothing ->
            defaultLayout $ do
                let signinWithGoogle = $(widgetFile "signin-with-google")
                let accounts = []
                headerWidget Nothing
                homeWidget Nothing

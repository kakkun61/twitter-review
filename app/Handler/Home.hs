module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    mUser <- maybeAuth
    case mUser of
        Just user ->
            runDB $ do
                reps <- selectList [UserAccountRelationUserId ==. entityKey user] []
                $(logDebug) $ pack $ show reps
                accounts <- filterJust <$> mapM (get . userAccountRelationAccountId . entityVal) reps
                lift $ defaultLayout $ do
                    let signinWithGoogle = $(widgetFile "signin-with-google")
                    headerWidget $ Just $ entityVal user
                    $(widgetFile "home")
                where
                    filterJust (Just a : r)  = a : filterJust r
                    filterJust (Nothing : r) = filterJust r
                    filterJust []            = []
        Nothing ->
            defaultLayout $ do
                let signinWithGoogle = $(widgetFile "signin-with-google")
                let accounts = []
                headerWidget $ entityVal <$> mUser
                $(widgetFile "home")

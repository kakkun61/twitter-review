module Handler.MasterLogin where

import Import

getMasterLoginR :: Handler Html
getMasterLoginR = do
    $(logDebug) "getMasterLoginR"
    user <- entityVal <$> requireAuth
    defaultLayout $ do
        let signinWithGoogle = $(widgetFile "signin-with-google")
        headerWidget $ Just user
        $(widgetFile "master-login")

module Handler.MasterLogin where

import Import

getMasterLoginR :: Handler Html
getMasterLoginR = do
    $(logDebug) "getMasterLoginR"
    mUser <- maybeAuth
    defaultLayout $ do
        let signinWithGoogle = $(widgetFile "signin-with-google")
        headerWidget mUser
        $(widgetFile "master-login")

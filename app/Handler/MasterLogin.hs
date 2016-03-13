module Handler.MasterLogin where

import Import

getMasterLoginR :: Handler Html
getMasterLoginR = do
    $(logDebug) "getMasterLoginR"
    mUser <- fmap (fmap entityVal) maybeAuth
    defaultLayout $ do
        let signinWithGoogle = $(widgetFile "signin-with-google")
        $(widgetFile "header")
        $(widgetFile "master-login")

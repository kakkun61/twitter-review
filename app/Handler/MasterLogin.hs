module Handler.MasterLogin where

import Import

getMasterLoginR :: Handler Html
getMasterLoginR = do
    mUser <- maybeAuth
    defaultLayout $ do
        let signinWithGoogle = $(widgetFile "signin-with-google")
        headerWidget mUser
        $(widgetFile "master-login")

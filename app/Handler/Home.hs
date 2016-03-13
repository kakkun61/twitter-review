module Handler.Home where

import Import
import Database.Persist.Sql

getHomeR :: Handler Html
getHomeR = do
    mUser <- fmap (fmap entityVal) maybeAuth
    defaultLayout $ do
        let signinWithGoogle = $(widgetFile "signin-with-google")
        $(widgetFile "header")
        $(widgetFile "home")

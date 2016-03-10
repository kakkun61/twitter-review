module Handler.Home where

import Import
import Database.Persist.Sql

getHomeR :: Handler Html
getHomeR = do
    mUser <- fmap (fmap entityVal) maybeAuth
    let header = $(widgetFile "header")
    defaultLayout $(widgetFile "home")

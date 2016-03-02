module Handler.Home where

import Import
import Database.Persist.Sql

getHomeR :: Handler Html
getHomeR = do
    mAId <- maybeAuthId
    defaultLayout $(widgetFile "home")

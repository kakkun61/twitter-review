module Handler.NewAccount where

import Import

getNewAccountR :: Handler Html
getNewAccountR = do
    mUser <- fmap (fmap entityVal) maybeAuth
    let header = $(widgetFile "header")
    defaultLayout $(widgetFile "new-account")

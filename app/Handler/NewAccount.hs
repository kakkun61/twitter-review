module Handler.NewAccount where

import Import

getNewAccountR :: Handler Html
getNewAccountR = do
    mUser <- fmap (fmap entityVal) maybeAuth
    defaultLayout $ do
        $(widgetFile "header")
        $(widgetFile "new-account")

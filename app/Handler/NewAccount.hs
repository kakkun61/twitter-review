module Handler.NewAccount where

import Import

getNewAccountR :: Handler Html
getNewAccountR = do
    user <- entityVal <$> requireAuth
    defaultLayout $ do
        headerWidget $ Just user
        $(widgetFile "new-account")

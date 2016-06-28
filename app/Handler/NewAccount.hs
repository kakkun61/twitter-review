module Handler.NewAccount where

import Import

getNewAccountR :: Handler Html
getNewAccountR = undefined
-- getNewAccountR = do
--     user <- entityVal <$> requireAuth
--     defaultLayout $ do
--         headerWidget $ Just user
--         $(widgetFile "new-account")

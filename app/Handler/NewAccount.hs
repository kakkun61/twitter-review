module Handler.NewAccount where

import Import

getNewAccountR :: Handler Html
getNewAccountR = error "don't call me temporally"
-- getNewAccountR = do
--     user <- entityVal <$> requireAuth
--     defaultLayout $ do
--         headerWidget $ Just user
--         $(widgetFile "new-account")

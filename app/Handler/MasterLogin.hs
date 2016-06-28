module Handler.MasterLogin where

import Import

getMasterLoginR :: Handler Html
getMasterLoginR = error "don't call me temporally"
-- getMasterLoginR = do
--     $(logDebug) "getMasterLoginR"
--     mUser <- (entityVal <$>) <$> maybeAuth
--     defaultLayout $ do
--         let signinWithGoogle = $(widgetFile "signin-with-google")
--         headerWidget mUser
--         $(widgetFile "master-login")

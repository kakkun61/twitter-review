module Handler.MasterLogin where

import Import

getMasterLoginR :: Handler Html
getMasterLoginR = undefined
-- getMasterLoginR = do
--     $(logDebug) "getMasterLoginR"
--     mUser <- (entityVal <$>) <$> maybeAuth
--     defaultLayout $ do
--         let signinWithGoogle = $(widgetFile "signin-with-google")
--         headerWidget mUser
--         $(widgetFile "master-login")

module Template where

import ClassyPrelude.Yesod
import Yesod.Auth
import Foundation
import Settings
import Settings.StaticFiles
import Model.Table.Account
import Model.Table.User

headerWidget :: Maybe User -> Widget
headerWidget mUser = $(widgetFile "header")

homeWidget :: Maybe User -> Widget
homeWidget mUser = do
  let signinWithGoogle = $(widgetFile "signin-with-google")
  $(widgetFile "home")

accountSettingWidget :: Account -> Widget
accountSettingWidget account = do
    go $ screenName account
    where
        go screenName = $(widgetFile "account-setting")

-- newtype TweetFormData = TweetFormData { tweetFormText :: Text }
--     deriving Show
--
-- tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
-- tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

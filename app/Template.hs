module Template where

import ClassyPrelude.Yesod
import Yesod.Auth
import Foundation
import Settings
import Settings.StaticFiles
import Model.Table.Account (Account)
import qualified Model.Table.Account as Account
import Model.Table.User (User)
import qualified Model.Table.User as User

headerWidget :: Maybe User -> Widget
headerWidget mUser = $(widgetFile "header")

homeWidget :: Maybe User -> [Account] -> Widget
homeWidget mUser accounts = do
  let signinWithGoogle = $(widgetFile "signin-with-google")
  $(widgetFile "home")

accountSettingWidget :: Account -> Widget
accountSettingWidget account = do
    let screenName = Account.screenName account
    $(widgetFile "account-setting")

-- newtype TweetFormData = TweetFormData { tweetFormText :: Text }
--     deriving Show
--
-- tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
-- tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

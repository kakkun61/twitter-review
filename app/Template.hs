module Template where

import ClassyPrelude.Yesod
import qualified Data.Set as Set
import Yesod.Auth
import Foundation
import Settings
import Settings.StaticFiles
import Model
import Model.Table.Account (Account)
import qualified Model.Table.Account as Account
import Model.Table.Tweet (Tweet)
import qualified Model.Table.Tweet as Tweet
import Model.Table.TweetCandidate (TweetCandidate)
import qualified Model.Table.TweetCandidate as TweetCandidate
import Model.Table.User (User)
import qualified Model.Table.User as User
import Model.Parameter.Status

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

accountWidget :: Account -> [(Tweet, TweetCandidate)] -> Set StatusParam -> Widget
accountWidget account rows statusParams = do
    let accountIdParam = Account.id account
    $(widgetFile "account")

newtype TweetFormData = TweetFormData { tweetFormText :: Text }
    deriving Show

tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

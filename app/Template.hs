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
import Model.Table.Comment (Comment)
import qualified Model.Table.Comment as Comment
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

tweetWidget :: Account
            -> User
            -> Tweet
            -> Maybe Text
            -> [Either (Comment, User) (TweetCandidate, User)]
            -> (Widget, Enctype)
            -> (Widget, Enctype)
            -> (Widget, Enctype)
            -> (Widget, Enctype)
            -> (Widget, Enctype)
            -> Widget
tweetWidget account
            tweetUser
            tweet
            tweetUri
            ccs
            (candidateFormWidget, candidateFormEnctype)
            (commentFormWidget, commentFormEnctype)
            (tweetFormWidget, tweetFormEnctype)
            (closeFormWidget, closeFormEnctype)
            (reopenFormWidget, reopenFormEnctype) = do
    $(widgetFile "tweet")

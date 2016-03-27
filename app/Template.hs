module Template where

import ClassyPrelude.Yesod
import Yesod.Auth

import Foundation
import Settings
import Model

headerWidget :: Maybe User -> Widget
headerWidget mUser = $(widgetFile "header")

newtype TweetFormData = TweetFormData { tweetFormText :: Text }
    deriving Show

tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

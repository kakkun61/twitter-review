module Template where

import ClassyPrelude.Yesod
-- import Yesod.Auth
--
import Foundation
import Settings
import Settings.StaticFiles
import Model.Table.User

headerWidget :: Maybe User -> Widget
headerWidget mUser = $(widgetFile "header")

homeWidget :: Maybe User -> Widget
homeWidget mUser = do
  let signinWithGoogle = $(widgetFile "signin-with-google")
  $(widgetFile "home")

-- newtype TweetFormData = TweetFormData { tweetFormText :: Text }
--     deriving Show
--
-- tweetForm :: Html -> MForm Handler (FormResult TweetFormData, Widget)
-- tweetForm = renderDivs $ TweetFormData <$> areq textField "Tweet" Nothing

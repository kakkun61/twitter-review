{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Text.Blaze
import Yesod.Auth.GoogleEmail2 (Token)

import Model.TweetStatus
import Model.Token ()

type ScreenName = String
-- type TweetId = Int
-- type User = String

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToMarkup TweetId where
    toMarkup = string . show

{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Text.Blaze
import Data.Text (splitOn)
import qualified Data.Set as S

import Model.TweetStatus
import Model.Permission

type AccountIdParam = Int64
type TweetIdParam = Int64

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToMarkup TweetId where
    toMarkup = string . show

class Param a where
    toParam :: a -> Text
    fromParam :: Text -> Maybe a

instance (Param a, Ord a) => Param (Set a) where
    toParam = mconcat . intersperse "|" . map toParam . S.toList
    fromParam = Just . S.fromList . mapMaybe fromParam . splitOn "|"

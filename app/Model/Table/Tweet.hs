{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.Tweet where

import Prelude (String)
import Database.HDBC.Query.TH
import Database.Relational.Query
import DataSource (defineTable)
import Data.Time.LocalTime
import Data.Int

$(defineTable "tweet")

data TweetNoId = TweetNoId { tweetNoIdAccountId :: !Int64, tweetNoIdUserId :: !Int64, tweetNoIdStatus :: !Int8, tweetNoIdCreated :: !LocalTime }
$(makeRecordPersistableDefault ''TweetNoId)

insertTweetNoId :: Insert TweetNoId
insertTweetNoId = typedInsert tableOfTweet (TweetNoId |$| accountId' |*| userId' |*| status' |*| created')

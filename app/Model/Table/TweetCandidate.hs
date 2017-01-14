{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.TweetCandidate where

import Prelude (String)
import Database.HDBC.Query.TH
import Database.Relational.Query
import DataSource (defineTable)
import Data.Time.LocalTime
import Data.Int

$(defineTable "tweet_candidate")

data TweetCandidateNoId = TweetCandidateNoId { tweetCandidateNoIdTweetId :: !Int64, tweetCandidateNoIdText :: !String, tweetCandidateNoIdUserId :: !Int64, tweetCandidateNoIdCreated :: !LocalTime }
$(makeRecordPersistableDefault ''TweetCandidateNoId)

insertTweetCandidateNoId :: Insert TweetCandidateNoId
insertTweetCandidateNoId = typedInsert tableOfTweetCandidate (TweetCandidateNoId |$| tweetId' |*| text' |*| userId' |*| created')

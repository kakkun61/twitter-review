{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.Comment where

import Database.HDBC.Query.TH
import Database.Relational.Query
import DataSource (defineTable)
import Data.Time.LocalTime
import Data.Int
import Data.String

$(defineTable "comment")

data CommentNoId = CommentNoId { commentNoIdTweetId :: !Int64
                               , commentNoIdText :: !String
                               , commentNoIdUserId :: !Int64
                               , commentNoIdCreated :: !LocalTime
                               }
$(makeRecordPersistableDefault ''CommentNoId)

insertCommentNoId :: Insert CommentNoId
insertCommentNoId = typedInsert tableOfComment (CommentNoId |$| tweetId' |*| text' |*| userId' |*| created')

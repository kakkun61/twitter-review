module Model.Tweet where

import ClassyPrelude.Yesod
import Data.Int
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Int
import Database.HDBC.Types (IConnection)
import Database.Relational.Query hiding (runInsert)
import qualified Model.Table.Tweet as Table
import Model
import Model.TweetStatus
import Yesod.Relational

data Tweet = Tweet { id :: !Int64
                   , accountId :: !Int64
                   , userId :: !Int64
                   , status :: !TweetStatus
                   , created :: !LocalTime
                   }

insert :: (IConnection (YesodRelationalConnection site)) => Int64 -> Int64 -> TweetStatus -> UTCTime -> YesodRelationalMonad site Integer
insert accountId userId status created =
    runInsert
        (typedInsert Table.tableOfTweet (Table.TweetNoId |$| Table.accountId' |*| Table.userId' |*| Table.status' |*| Table.created'))
        (Table.TweetNoId accountId userId (toTableValue status) (utcToLocalTime utc created))

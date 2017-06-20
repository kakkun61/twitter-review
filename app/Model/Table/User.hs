{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.User where

import Prelude (String, Maybe)
import Database.HDBC.Query.TH
import Database.Relational.Query
import DataSource (defineTable)

$(defineTable "user")

data UserNoId = UserNoId { userNoIdEmail :: String, userNoIdEmailUser :: String, userNoIdDisplayName :: Maybe String, userNoIdToken :: String }
$(makeRecordPersistableDefault ''UserNoId)

insertUserNoId :: Insert UserNoId
insertUserNoId = typedInsert tableOfUser (UserNoId |$| email' |*| emailUser' |*| displayName' |*| token')

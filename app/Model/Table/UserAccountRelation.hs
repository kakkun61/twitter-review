{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.UserAccountRelation where

import Data.Int
import Database.HDBC.Query.TH
import Database.Relational.Query
import DataSource (defineTable)

$(defineTable "user_account_relation")

data UserAccountRelationNoId = UserAccountRelationNoId { userAccountRelationNoIdUserId :: !Int64, userAccountRelationNoIdAccountId :: !Int64, userAccountRelationNoIdPermission :: !Int8 }
$(makeRecordPersistableDefault ''UserAccountRelationNoId)

insertUserAccountRelationNoId :: Insert UserAccountRelationNoId
insertUserAccountRelationNoId = typedInsert tableOfUserAccountRelation (UserAccountRelationNoId |$| userId' |*| accountId' |*| permission')

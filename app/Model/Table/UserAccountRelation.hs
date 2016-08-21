{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.UserAccountRelation where

import DataSource (defineTable)

$(defineTable "user_account_relation")


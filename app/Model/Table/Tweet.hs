{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.Tweet where

import Prelude (String, Maybe)
import Database.HDBC.Query.TH
import Database.Relational.Query
import DataSource (defineTable)

$(defineTable "tweet")

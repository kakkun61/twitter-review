{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.Account where

import DataSource (defineTable)

$(defineTable "account")

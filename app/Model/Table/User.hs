{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.User where

import DataSource (defineTable)

$(defineTable "user")

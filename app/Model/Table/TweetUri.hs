{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Table.TweetUri where

import DataSource (defineTable)

$(defineTable "tweet_uri")

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Relational
    ( YesodRelational (..)
    , YesodRelationalMonad
    , runQuery
    , runQuery'
    , runUpdate
    , runInsert
    , run
    ) where

import           Database.HDBC.Types (IConnection)
import qualified Database.HDBC.Record.Query as R (runQuery, runQuery')
import qualified Database.HDBC.Record.Update as R (runUpdate)
import qualified Database.HDBC.Record.Insert as R (runInsert)
import           Database.HDBC.SqlValue (SqlValue)
import           Database.Relational.Query (Query, Update, Insert)
import           Database.Record.ToSql (ToSql)
import           Database.Record.FromSql (FromSql)

import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.IO.Class (liftIO)

import Yesod.Core.Handler (HandlerT)

type YesodRelationalMonad site = ReaderT (YesodRelationalConnection site) (HandlerT site IO)

class Monad (YesodRelationalMonad site) => YesodRelational site where
    type YesodRelationalConnection site :: *
    runRelational :: YesodRelationalMonad site a -> HandlerT site IO a

runQuery :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p, FromSql SqlValue a) => Query p a -> p -> YesodRelationalMonad site [a]
runQuery query param = ask >>= \conn -> liftIO $ R.runQuery conn query param

runQuery' :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p, FromSql SqlValue a) => Query p a -> p -> YesodRelationalMonad site [a]
runQuery' query param = ask >>= \conn -> liftIO $ R.runQuery' conn query param

runUpdate :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p) => Update p -> p -> YesodRelationalMonad site Integer
runUpdate update param = ask >>= \conn -> liftIO $ R.runUpdate conn update param

runInsert :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p) => Insert p -> p -> YesodRelationalMonad site Integer
runInsert insert param = ask >>= \conn -> liftIO $ R.runInsert conn insert param

run :: (IConnection (YesodRelationalConnection site)) => (YesodRelationalConnection site -> IO a) -> YesodRelationalMonad site a
run f = ask >>= (liftIO . f)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Relational
    ( YesodRelational
    , YesodRelationalConnection
    , runRelational
    , runQuery
    ) where

import           Database.HDBC.Types (IConnection)
import qualified Database.HDBC.Record.Query as R (runQuery)
import           Database.HDBC.SqlValue (SqlValue)
import           Database.Relational.Query (Query)
import           Database.Record.ToSql (ToSql)
import           Database.Record.FromSql (FromSql)

import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.IO.Class (liftIO)

import Yesod.Core.Handler (HandlerT)

type YesodRelationalMonad site = ReaderT (YesodRelationalConnection site) (HandlerT site IO)

class Monad (YesodRelationalMonad site) => YesodRelational site where
    type YesodRelationalConnection site
    runRelational :: YesodRelationalMonad site a -> HandlerT site IO a

runQuery :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p, FromSql SqlValue a) => Query p a -> p -> YesodRelationalMonad site [a]
runQuery query param = ask >>= \conn -> liftIO $ R.runQuery conn query param

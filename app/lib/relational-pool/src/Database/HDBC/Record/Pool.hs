{-# LANGUAGE FlexibleContexts #-}

module Database.HDBC.Record.Pool
 ( runQueryPool
 , createQueryPool
 ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Pool (withResource, Pool, createPool)
import Database.HDBC (IConnection, SqlValue, disconnect)
import Database.Record.ToSql (ToSql)
import Database.Record.FromSql (FromSql)
import Database.HDBC.Record (runQuery)
import Database.Relational.Query (Query)

runQueryPool :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a, MonadBaseControl IO m, MonadIO m)
             => Query p a
             -> p
             -> Pool conn
             -> m [a]
runQueryPool p q pool = withResource pool $ \conn -> liftIO $ runQuery conn p q

createQueryPool :: IConnection a
                => IO a
                -> Int
                -> IO (Pool a)
createQueryPool create = createPool create disconnect 1 10

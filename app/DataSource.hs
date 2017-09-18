module DataSource
  ( connectDB
  , defineTable
  ) where

import Database.HDBC.Query.TH (defineTableFromDB')
import Database.HDBC.MySQL
import Database.HDBC.Schema.MySQL (driverMySQL)
import Database.Relational.Query (identifierQuotation, IdentifierQuotation (..))
import Database.Relational.Schema.MySQLInfo.Config (config)
import Language.Haskell.TH (Q, Dec)
import System.IO (IO)
import Data.String (String)
import Text.Read (read)
import Text.Show (Show)
import Data.Maybe (Maybe (Nothing))
import System.Environment (getEnv)
import Control.Applicative ((<$>))
import Prelude (id)

connectDB :: IO Connection
connectDB = do
    let mysqlUnixSocket = ""
        mysqlGroup = Nothing
    mysqlHost     <- getEnv "TWITTER_REVIEW_DB_PORT_3306_TCP_ADDR"
    mysqlPort     <- read <$> getEnv "TWITTER_REVIEW_DB_PORT_3306_TCP_PORT"
    mysqlDatabase <- getEnv "TWITTER_REVIEW_DB_ENV_MYSQL_DATABASE"
    mysqlUser     <- getEnv "TWITTER_REVIEW_DB_ENV_MYSQL_USER"
    mysqlPassword <- getEnv "TWITTER_REVIEW_DB_ENV_MYSQL_PASSWORD"
    connectMySQL MySQLConnectInfo { .. }

connectDB' :: IO Connection
connectDB' = do
    let mysqlUser = "root"
        mysqlDatabase = "INFORMATION_SCHEMA"
        mysqlUnixSocket = ""
        mysqlGroup = Nothing
    mysqlHost     <- getEnv "TWITTER_REVIEW_DB_PORT_3306_TCP_ADDR"
    mysqlPassword <- getEnv "TWITTER_REVIEW_DB_ENV_MYSQL_ROOT_PASSWORD"
    mysqlPort     <- read <$> getEnv "TWITTER_REVIEW_DB_PORT_3306_TCP_PORT"
    connectMySQL MySQLConnectInfo { .. }

defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB'
        connectDB'
        (config { identifierQuotation = Quotation '`' })
        driverMySQL
        "twitter-review"
        tableName
        [''Show]

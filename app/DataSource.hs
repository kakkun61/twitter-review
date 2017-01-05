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
import Data.Maybe (Maybe (Nothing), maybe)
import System.Environment (lookupEnv)
import Control.Applicative ((<$>))
import Control.Monad ((>>=))
import Prelude (id, putStrLn)

connectDB :: IO Connection
connectDB = do
    let mysqlUser = "root"
        mysqlDatabase = "INFORMATION_SCHEMA"
        mysqlUnixSocket = ""
        mysqlGroup = Nothing
    mysqlHost     <- maybe "" id <$> lookupEnv "TWITTER_REVIEW_DB_PORT_3306_TCP_ADDR"
    mysqlPassword <- maybe "" id <$> lookupEnv "TWITTER_REVIEW_DB_ENV_MYSQL_ROOT_PASSWORD"
    mysqlPort     <- maybe 0 read <$> lookupEnv "TWITTER_REVIEW_DB_PORT_3306_TCP_PORT"
    connectMySQL MySQLConnectInfo { .. }

defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB'
        connectDB
        (config { identifierQuotation = Quotation '`' })
        driverMySQL
        "twitter-review"
        tableName
        [''Show]

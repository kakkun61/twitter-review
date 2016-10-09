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
import Text.Show (Show)
import Data.Maybe (maybe)
import System.Environment (lookupEnv)
import Control.Applicative ((<$>))
import Prelude (id, putStrLn)

connectDB :: IO Connection
connectDB = do
    password <- maybe "" id <$> lookupEnv "MYSQL_PASSWORD"
    connectMySQL defaultMySQLConnectInfo { mysqlDatabase = "INFORMATION_SCHEMA"
                                         , mysqlPassword = password
                                         }

defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB'
        connectDB
        (config { identifierQuotation = Quotation '`' })
        driverMySQL
        "twitter-review"
        tableName
        [''Show]

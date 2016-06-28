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

connectDB :: IO Connection
connectDB = connectMySQL defaultMySQLConnectInfo { mysqlDatabase = "INFORMATION_SCHEMA"}

defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB'
        connectDB
        (config { identifierQuotation = Quotation '`' })
        driverMySQL
        "twitter-review"
        tableName
        [''Show]

module DataSource
  ( connectDB
  , defineTable
  ) where

import Database.HDBC.Query.TH (defineTableFromDB')
import Database.HDBC.MySQL
import Database.HDBC.Schema.MySQL (driverMySQL)
import Database.Relational.Query.Component (defaultConfig, normalizedTableName)
import Database.Record.TH (derivingShow)
import Language.Haskell.TH (Q, Dec)
import System.IO (IO)
import Data.String (String)
import Data.Bool (Bool(False))

connectDB :: IO Connection
connectDB = connectMySQL defaultMySQLConnectInfo { mysqlDatabase = "INFORMATION_SCHEMA"}

defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB'
        connectDB
        (defaultConfig { normalizedTableName = False })
        driverMySQL
        "twitter-review"
        tableName
        [derivingShow]

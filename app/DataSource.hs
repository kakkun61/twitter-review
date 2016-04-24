module DataSource
  ( connect
  , defineTable
  ) where

import Database.HDBC.Query.TH (defineTableFromDB')
import Database.HDBC.MySQL
import Database.HDBC.Schema.MySQL (driverMySQL)
import Database.Relational.Query.Component (defaultConfig, normalizedTableName)
import Database.Record.TH (derivingShow)
import Language.Haskell.TH (Q, Dec)

connect :: IO Connection
connect = connectMySQL defaultMySQLConnectInfo { mysqlDatabase = "INFORMATION_SCHEMA"}

defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB'
        connect
        (defaultConfig { normalizedTableName = False })
        driverMySQL
        "twitter-review"
        tableName
        [derivingShow]

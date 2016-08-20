module Database.Relational.Query.MySQL
    ( selectLastInsertId
    ) where

import Data.Int (Int64)
import Database.Relational.Query (Query)
import Database.Relational.Query.Type (unsafeTypedQuery)

-- | @SELECT LAST_INSERT_ID()@
selectLastInsertId :: Query () Int64
selectLastInsertId = unsafeTypedQuery "SELECT LAST_INSERT_ID()"

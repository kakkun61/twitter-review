module Model.Permission where

import ClassyPrelude.Yesod
import Database.Persist.Sql

data Permission = Admin | ReadWrite
                  deriving (Show, Read, Eq)

instance PersistField Permission where
    toPersistValue Admin     = PersistInt64 0
    toPersistValue ReadWrite = PersistInt64 1

    fromPersistValue (PersistInt64 0) = Right Admin
    fromPersistValue (PersistInt64 1) = Right ReadWrite
    fromPersistValue _                = Left "only PersistInt64 can be converted to Permission"

instance PersistFieldSql Permission where
    sqlType _ = SqlInt32

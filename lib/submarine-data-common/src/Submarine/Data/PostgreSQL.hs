module Submarine.Data.PostgreSQL where
import Data.Pool
import Database.PostgreSQL.Simple
import Data.Text (unpack)

import Submarine.Data.Config
import Submarine.Data.Pool

initializePostgresConnectionPool :: PostgresConfig -> IO (Pool Connection)
initializePostgresConnectionPool conf = do
	let connectInfo = ConnectInfo
		{ connectHost     = unpack $ postgresConfigHost conf
		, connectPort     = fromIntegral $ postgresConfigPort conf
		, connectUser     = unpack $ postgresConfigUsername conf
		, connectPassword = unpack $ postgresConfigPassword conf
		, connectDatabase = unpack $ postgresConfigDatabase conf
		}
	poolDefaults (connect connectInfo) close

type PostgreSQL a = Connection -> IO a
class PostgreSQLBacked m where
	pg :: PostgreSQL a -> m a
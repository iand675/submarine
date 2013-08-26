module Submarine.Data.PostgreSQL where
import Database.PostgreSQL.Simple

import Submarine.Data.Config
import Submarine.Data.Pool

initializePostgresConnectionPool :: PostgresConfig -> IO (Pool Connection)
initializePostgresConnectionPool conf = do
	let connectInfo = ConnectInfo
		{ connectHost     = unpack $ postgresConfigHost conf
		, connectPort     = fromIntegral $ postgresConfigPort port
		, connectUser     = unpack $ postgresConfigUsername conf
		, connectPassword = unpack $ postgresConfigPassword conf
		, connectDatabase = unpack $ postgresConfigDatabase conf
		}
	poolDefaults (connect connectInfo) close

class PostgreSQLBacked m where
	pg :: PostgreSQL a -> m a
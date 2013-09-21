module Submarine.Data.PostgreSQL where
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.UUID
import Data.Text (unpack)

import Submarine.Common.Models
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

{-data Migration = Migration-}
	{-{ migrationName :: Text-}
	{-, migrationDependsOn :: Maybe Text-}
	{-, migrationActions :: [MigrationAction]-}
	{-}-}

{-data MigrationAction-}
	{-= CreateTable-}
		{-{ tableName :: Text-}
		{-, force-}
		{-, columns-}
		{-, indexes-}
	{-| AddColumn-}
	{-| AddIndex-}

-- SELECT table_name FROM information_schema.tables WHERE table_schema = 'TASKS';
-- create table if not exists migrations
-- select latest migration from migrations
-- run any migrations in order that don't exist

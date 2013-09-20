{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Submarine.Data.Users where
import Control.Applicative
import Crypto.PasswordStore
import Data.ByteString (ByteString)
import Data.Pool
import Data.Text (Text)
import Data.Text.Encoding
import Database.PostgreSQL.Simple hiding (Connection)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.SqlQQ
import Database.Redis as Redis (Connection)
import Submarine.Common.Models
import Submarine.Data.Util
import Submarine.Errors
import Submarine.Models.Accounts

hashPassword :: Text -> IO ByteString
hashPassword t = makePassword (encodeUtf8 t) 14

validatePassword :: Text -> ByteString -> Bool
validatePassword t b = verifyPassword (encodeUtf8 t) b

data DbUser = DbUser
	{ dbUserUsername     :: Text
	, dbUserEmail        :: Text
	, dbUserPasswordHash :: ByteString
	}

instance FromRow DbUser where
	fromRow = DbUser <$> field <*> field <*> field

instance ToRow DbUser where
	toRow (DbUser u e p) = [toField u, toField e, toField p]

initializeUser :: NewUser -> ByteString -> DbUser
initializeUser u h = DbUser
	{ dbUserUsername     = newUserUsername u
	, dbUserEmail        = newUserEmail u
	, dbUserPasswordHash = h
	}

fromDbUser :: DbUser -> User
fromDbUser u = User
	{ userUsername          = dbUserUsername u
	, userEmail             = dbUserEmail u
	, userName              = ""
	, userGithubTokens      = []
	, userPhone             = ""
	, userTaskCreationEmail = ""
	, userStripeInfo        = ()
	, userFogbugzInfo       = ()
	}

type Single = Either SingleValueError
type UserPatch = User
data UserBackend m = UserBackend
  { createUser :: NewUser -> m (Single User)
  , getUser    :: Text -> m (Single (Maybe User))
  , updateUser :: Text -> UserPatch -> m (Single (Maybe User))
  , deleteUser :: Text -> m ()
  , listUsers  :: UserQuery -> m [User]
  }

type PGPool = Pool Postgres.Connection
createUserImpl :: PGPool -> NewUser -> IO (Single User)
createUserImpl p n = withResource p $ \c -> do
	hashed <- hashPassword $ newUserPassword n
	let initialized = initializeUser n hashed
	u <- execute c [sql|
		insert into users (username, email, password_hash) values (?, ?, ?)
	|] initialized
	return $! Right $ fromDbUser initialized

getUserImpl :: PGPool -> Text -> IO (Single (Maybe User))
getUserImpl p username = withResource p $ \c -> do
	us <- query c [sql|
		select username, email, password_hash from users where username = ?
	|] (Only username)
	return $! oneOrNothing $ map fromDbUser us

listUsersImpl :: PGPool -> UserQuery -> IO [User]
listUsersImpl p q = withResource p $ \c -> do
	 us <- query_ c [sql| select username, email, password_hash from users |]
	 return $! map fromDbUser us

initializeUserBackend :: Redis.Connection -> PGPool -> UserBackend IO
initializeUserBackend redisConn pgConn = postgresBackend pgConn

postgresBackend :: PGPool -> UserBackend IO
postgresBackend c = UserBackend
	{ createUser = createUserImpl c
	, getUser    = getUserImpl c
	, listUsers  = listUsersImpl c
	}

module Database.PostgreSQL.Simple.UUID where
import Blaze.ByteString.Builder.Char8 (fromString)
import qualified Data.ByteString.Char8 as C
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple.BuiltinTypes ( BuiltinType(UUID), builtin2oid )
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

instance FromField UUID where
   fromField f mdata =
      if typeOid f /= builtin2oid UUID
        then returnError Incompatible f ""
        else case C.unpack `fmap` mdata of
         Nothing  -> returnError UnexpectedNull f ""
         Just dat ->
            case [ x | (x,t) <- reads dat, ("","") <- lex t ] of
              [x] -> return x
              _   -> returnError ConversionFailed f dat

instance ToField UUID where
  toField = Plain . inQuotes . fromString . UUID.toString
module Data.PGSchema where

data PostgresType
  = BigInt
	| BigSerial
	| Bit Int
	| BitVarying Int
	| Boolean
	| Box
	| ByteArray
	| Character Int
	| CharacterVarying Int
	| NetworkAddress
	| Circle
	| Date
	| DoublePrecision
	| HostAddress
	| Integer
	| Interval -- TODO: figure out how this works
	| Line
	| LineSegment
	| MacAddress
	| Money
	| Numeric Precision Scale
	| Path
	| Point
	| Polygon
	| Real
	| SmallInt
	| SmallSerial
	| Serial
	| Text
	| Time -- TODO (p) (with|without) timezone
	| Timestamp -- TODO (p) (with|without) timezone
	| TextSearchQuery
	| TextSearchVector
	| TransactionIdSnapshot
	| UUID
	| XML
	| JSON
schema :: Schema ()
schema = do
	table "task" $ do
		def $ (column "id" $ NotNull UUID) # (defaultValue .= ____)
		def $ (column "name" $ NotNull Text)
		def $ (column "list_id" $ UUID) # (foreignKey .= "list.id")
		def $ (column "created_by" $ UUID) # (foreignKey .= "user.id")
	table "list" $ do
		def $ (column "id" $ NotNull UUID) # (defaultValue .= ____)
		def $ (column "name" $ NotNull Text)
		def $ (column "category_id" UUID)
	table
	table



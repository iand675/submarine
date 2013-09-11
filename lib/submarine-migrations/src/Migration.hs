module Migration where
{-
data NonNull
data Nullable
data ColumnType n a where
	DbText :: ColumnType Text
	DbInt :: ColumnType Int
	DbDouble :: ColumnType Double
	DbDecimal :: ColumnType Integer
	DbDateTime :: ColumnType DateTime
	DbTimestamp :: ColumnType Timestamp
	DbTime :: ColumnType Time
	DbDate :: ColumnType Date
	DbBinary :: ColumnType ByteString
	DbBool :: ColumnType Bool
	DbCustom :: (Generic a, Typeable a) => a -> ColumnType a
	DbNullable :: ColumnType NonNull a -> ColumnType Nullable (Maybe a)
-}
--data Table name columns = 


--createTable :: (Generic a, Typeable a, Generic b, Typeable b) => Text -> (TableDefinition a -> TableDefinition b) -> Something
--dropTable
--changeTable
--renameTable
--addColumn :: TableDefinition (HList t) -> ColumnType n a -> TableDefinition (HList (ColumnType n a ': t))
--addColumn = return ()

--renameColumn :: (Contains (HList t) oldColName, Contains (HList t') newColName) => oldColName -> newColName -> TableDefinition (HList t) -> TableDefinition (HList t')
--changeColumn
--removeColumn
--addIndex
--removeIndex

createTable :: Text -> [NewColumn] -> DbAction
createTable name columns = "CREATE TABLE " <> name <> "(" <> buildColumns columns <> ");"

buildColumns :: [NewColumn] -> Text
buildColumns = intercalate "," . map buildColumn

buildColumn :: Column -> Text
buildColumn c = columnName c <> " " <> buildType (columnType c)

-- data TableState = Identical | Different [Difference]
-- data Difference = Difference { diffColumnName :: Text, diffColumnChange :: Text, diffIsAcceptable :: Bool }
-- diffTableDefinition :: TableDefinition a -> TableDefinition b -> TableState
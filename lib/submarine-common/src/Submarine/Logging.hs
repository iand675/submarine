module Submarine.Logging where
import System.Log.FastLogger

data LogEntry = LogEntry
	{ entryType  :: Text
	, entryLevel :: LogLevel
	, entryValue :: Builder
	}

data LogLevel = Debug | Error | Fatal | Info | Trace | Warn

class Loggable a where
	toLogEntry :: a -> LogEntry

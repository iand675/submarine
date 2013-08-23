module NewRelic where

-- agent.rb
data LicenseException
data ForceDisconnectException
data ForceRestartException
data UnrecoverableServerException
data UnrecoverableAgentException
data ServerError
data BackgroundLoadingError

unknownMetric = "(unknown)"

recordMetric :: MetricName -> Metric -> NewRelic ()
incrementMetric :: MetricName -> NewRelic ()
incrementMetric' :: Num a => MetricName -> a -> NewRelic ()

startNewRelicAgent :: ___
resetStats :: NewRelic ()
shutdownNewRelicAgent :: ___

-- addInstrumentation
-- setSqlObfuscator
-- disableSqlRecording
-- subscribeToNewRelicEvents
-- notifyNewRelicEvent
-- disableTransactionTracing
-- abortTransaction
-- disableAllTracing
-- isExecutionTraced
-- isSqlRecorded
-- ignoreErrorFilter
-- noticeError
-- addCustomParameters
-- setUserAttributes
-- setTransactionName
-- getTransactionName
-- withDatabaseMetricName
-- recordTransaction
-- browserTimingHeader
-- browserTimingFooter
-- registerReportChannel

-- version.rb
version = "3.6.6"

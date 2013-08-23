{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AMQP where
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text (Text)
import Data.String
import Data.Word
import Network.AMQP.Types
import qualified Network.AMQP as A

class FromMessage a where
  fromMessage :: A.Message -> Maybe a

runAMQP :: A.Channel -> AMQP a -> IO a
runAMQP chan action = runReaderT (fromAMQP action) chan

transactionally :: AMQP (Either e a) -> AMQP (Either e a)
transactionally action = do
	txSelect
	result <- action
	case result of
		(Left err) -> txRollback
		(Right ok) -> txCommit
	return result

newtype AMQP a = AMQP { fromAMQP :: ReaderT A.Channel IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype Queue a = QueueName { fromQueueName :: Text }
  deriving (IsString, Read, Show, Eq)

newtype Exchange = ExchangeName { fromExchangeName :: Text }
  deriving (IsString, Read, Show, Eq)

newtype RoutingKey a = RoutingKey { fromRoutingKey :: Text }
  deriving (IsString, Read, Show, Eq)

qos :: Word32 -> Word16 -> AMQP ()
qos prefetchSize prefetchCount = AMQP $ ask >>= \chan -> (liftIO $ A.qos chan prefetchSize prefetchCount)

setPrefetchCount :: Word16 -> AMQP ()
setPrefetchCount = qos 0

declareExchange :: A.ExchangeOpts -> AMQP ()
declareExchange exchangeOpts = AMQP (ask >>= \c -> (liftIO $ A.declareExchange c exchangeOpts))

deleteExchange :: Exchange -> AMQP ()
deleteExchange exchange = AMQP (ask >>= \c -> liftIO $ A.deleteExchange c $ fromExchangeName exchange)

declareQueue :: A.QueueOpts -> AMQP (Text, Int, Int)
declareQueue queueOpts = AMQP (ask >>= \c -> liftIO $ A.declareQueue c queueOpts)

bindQueue :: Queue a -> Exchange -> RoutingKey a -> AMQP ()
bindQueue queue exchange routingKey = AMQP (ask >>= \c -> liftIO $ A.bindQueue c (fromQueueName queue) (fromExchangeName exchange) (fromRoutingKey routingKey))

bindQueue' :: Queue a -> Exchange -> RoutingKey a -> FieldTable -> AMQP ()
bindQueue' queue exchange routingKey fieldTable = AMQP (ask >>= \c -> liftIO $ A.bindQueue' c (fromQueueName queue) (fromExchangeName exchange) (fromRoutingKey routingKey) fieldTable)

purgeQueue :: Queue a -> AMQP Word32
purgeQueue queue = AMQP (ask >>= \c -> liftIO $ A.purgeQueue c (fromQueueName queue))

deleteQueue :: Queue a -> AMQP Word32
deleteQueue queue = AMQP (ask >>= \c -> liftIO $ A.deleteQueue c (fromQueueName queue))

publishMessage :: Exchange -> RoutingKey a -> A.Message -> AMQP ()
publishMessage exchange routingKey message = AMQP (ask >>= \c -> liftIO $ A.publishMsg c (fromExchangeName exchange) (fromRoutingKey routingKey) message)

getMessage :: A.Ack -> Queue a -> AMQP (Maybe (A.Message, A.Envelope))
getMessage ack queue = AMQP (ask >>= \c -> liftIO $ A.getMsg c ack $ fromQueueName queue)

rejectMessage :: LongLongInt -> Bool -> AMQP ()
rejectMessage deliveryTag requeue = AMQP (ask >>= \c -> liftIO $ A.rejectMsg c deliveryTag requeue)

recoverMessages :: Bool -> AMQP ()
recoverMessages requeue = AMQP (ask >>= \c -> liftIO $ A.recoverMsgs c requeue)

ackMessage :: LongLongInt -> Bool -> AMQP ()
ackMessage deliveryTag multiple = AMQP (ask >>= \c -> liftIO $ A.ackMsg c deliveryTag multiple)

ackEnvelope :: A.Envelope -> AMQP ()
ackEnvelope = AMQP . liftIO . A.ackEnv

txSelect :: AMQP ()
txSelect = AMQP (ask >>= liftIO . A.txSelect)

txCommit :: AMQP ()
txCommit = AMQP (ask >>= liftIO . A.txCommit)

txRollback :: AMQP ()
txRollback = AMQP (ask >>= liftIO . A.txRollback)

flow :: Bool -> AMQP ()
flow flag = AMQP (ask >>= \c -> liftIO $ A.flow c flag)

data Diff a = Diff { old :: a, new :: a }


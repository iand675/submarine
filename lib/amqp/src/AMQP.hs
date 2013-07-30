module AMQP where
import Data.Aeson
import Network.AMQP

class FromMessage a where
  fromMessage :: Message -> Maybe a

instance FromMessage a where
  fromMessage = decode

newtype AMQP a = AMQP { fromAMQP :: ReaderT Channel IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype Queue = QueueName { fromQueueName :: Text }
  deriving (IsString, Read, Show, Eq)

newtype Exchange = ExchangeName { fromExchangeName :: Text }
  deriving (IsString, Read, Show, Eq)

newtype RoutingKey a = RoutingKey { fromRoutingKey :: Text }
  deriving (IsString, Read, Show, Eq)

qos :: Word32 -> Word16 -> AMQP ()
declareExchange :: ExchangeOpts -> AMQP ()
deleteExchange :: Exchange -> AMQP ()
declareQueue :: QueueOpts -> AMQP (Text, Int, Int)
bindQueue :: Queue -> Exchange -> RoutingKey a -> AMQP ()
bindQueue' :: Qeuue -> Exchange -> RoutingKey a -> FieldTable -> AMQP ()
purgeQueue :: Queue -> AMQP Word32
deleteQueue :: Queue -> AMQP Word32
-- consumeMsgs :: 
-- cancelConsumer
publishMessage :: Exchange -> RoutingKey a -> Message -> AMQP ()
getMessage :: Ack -> Queue -> AMQP (Maybe (Message, Envelope))
rejectMessage :: LongLongInt -> Bool -> AMQP ()
recoverMessages :: Bool -> AMQP ()
ackMessage :: LongLongInt -> Bool -> AMQP ()
ackEnvelope :: Envelope -> AMQP ()
txSelect :: AMQP ()
txCommit :: AMQP ()
txRollback :: AMQP ()
flow :: Bool -> AMQP ()

{-
getMessageS :: Proxy p => Channel -> Ack -> Text -> Producer p (Maybe (Message, Envelope)) IO r
publishMessageD ::  Proxy p => Channel -> Text -> Text -> p a' Message b' () IO r
-}

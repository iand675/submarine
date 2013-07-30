module AMQP where
import Data.Aeson
import Network.AMQP

class FromMessage a where
  fromMessage :: Message -> Maybe a

instance FromMessage a where
  fromMessage = decode

getMessageS :: Proxy p => Channel -> Ack -> Text -> Producer p (Maybe (Message, Envelope)) IO r
publishMessageD ::  Proxy p => Channel -> Text -> Text -> p a' Message b' () IO r

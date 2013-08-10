module Router where
import EventHandler

router = Route "/stripe_event" $ runRoute eventHandler


module Submarine.AMQP.Schema where
import AMQP
import Submarine.Models.Account
import Submarine.Models.Task

newtype Exchange
newtype Topic
newtype Queue

userCreated =
	account creation email
	
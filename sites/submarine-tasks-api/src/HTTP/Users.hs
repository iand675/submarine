{-# LANGUAGE OverloadedStrings #-}
module HTTP.Users where

import Control.Applicative
import Control.Monad.Reader
import Database.Redis.Simple hiding (Message)
import Data.Either
import Data.Monoid
import Data.Pool
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Network.HTTP.Types
import Submarine.Web.Actions
import Submarine.Web.Message
import qualified Web.Scotty as S

import Config
import Util
import Submarine.Models.Accounts

createUserHandler :: Handler
createUserHandler = do
	jsonData >>= createUser >>= whenOk json

getUserHandler :: Handler
getUserHandler = do
	param "userId" >>= getUser >>= returnSingle

listUsersHandler = do
	listUsers >>= json

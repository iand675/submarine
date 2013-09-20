{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Submarine.Models.Accounts where
import Data.Text (Text)

import Submarine.Common.Models
import Submarine.JSON

type StripeInfo = ()
type FogBugzInfo = ()

data User = User
  { userUsername          :: Text
	, userEmail             :: Text
  , userName              :: Text
  , userGithubTokens      :: [Text]
  , userPhone             :: Text
  , userTaskCreationEmail :: Text
  , userStripeInfo        :: StripeInfo
  , userFogbugzInfo       :: FogBugzInfo
  }

type instance Id User = Int
jsonize ''User

data NewUser = NewUser
	{ newUserUsername :: Text
	, newUserEmail    :: Text
	, newUserPassword :: Text
	{-, newUserName     :: Text-}
	}
jsonize ''NewUser

data UserQuery = All

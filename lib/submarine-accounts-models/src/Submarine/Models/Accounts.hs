{-# LANGUAGE TemplateHaskell #-}
module Submarine.Models.Accounts where
import Submarine.Common.Models
import Submarine.JSON

type StripeInfo = ()
type FogBugzInfo = ()

data User = User
  { userEmail             :: Text
  , userPasswordHash      :: Text
  , userName              :: Text
  , userGithubTokens      :: [Text]
  , userPhone             :: Text
  , userTaskCreationEmail :: Text
  , userStripeInfo        :: StripeInfo
  , userFogbugzInfo       :: FogBugzInfo
  }

jsonize ''User
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module GitHub.Types where
import Control.Applicative
import Control.Monad.RWS.Strict
import Data.ByteString.Char8 (ByteString, append, pack)
import Data.Text (Text)
import Network.HTTP.Conduit

data APILocation = GitHub | Enterprise String

githubConfig :: APILocation -> Maybe String -> Maybe (Request m)
githubConfig loc mk = do
  req <- parseUrl endpoint
  return $ addTokenHeader req
  where
    endpoint = case loc of
      GitHub -> "https://api.github.com/"
      Enterprise url -> url
    addTokenHeader = case mk of
      Nothing -> id
      Just k -> \req -> req
        { requestHeaders = ("Authorization", append "token " (pack k)) : requestHeaders req
        }

data RateLimit = RateLimit
  { rateLimitLimit :: !Int
  , rateLimitRemaining :: !Int
  }

data GitHubConfig m = GitHubConfig
  { manager :: Manager
  , baseRequest :: Request m
  }

newtype GitHub a = GitHubM { runGitHub :: RWST (GitHubConfig IO) () RateLimit IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

data PublicKey = PublicKey
  { publicKeyId :: Int
  , publicKeyKey :: Text
  }

data CurrentUserKey = CurrentUserKey
  { currentUserKeyId :: Int
  , currentUserKeyKey :: Text
  , currentUserKeyUrl :: Text
  , currentUserKeyTitle :: Text
  }

data NewUserKey = NewUserKey
  { newUserKeyTitle :: Text
  , newUserKeyKey :: Text
  }

data UserKeyPatch = UserKeyPatch
  { keyPatchTitle :: Text
  , keyPatchKey :: Text
  }

data EventsData
data OwnerName
data RepoName
data OrgName
data UserName


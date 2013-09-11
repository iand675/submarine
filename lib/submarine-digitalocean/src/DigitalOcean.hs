{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts #-}
module DigitalOcean where
import qualified API
import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.Text as T
import Data.List
import Network.HTTP.Conduit hiding (port)
import URI.TH
import URI.Types

type DropletId = Int
type ImageId = Int
type RegionId = Int
type SizeOptionId = Int
type EventId = Int
type DomainId = Int
type SshKeyId = Int
type RecordId = Int
type IpAddress = T.Text
type NewSshKey = T.Text
type NewDomain = T.Text

data Credentials = Credentials
	{ _credentialsClientId :: T.Text
	, _credentialsApiKey :: T.Text
	} deriving (Read, Show, Eq)

data Droplet = Droplet
	{ _dropletBackupsActive :: Maybe Bool
	, _dropletId :: DropletId
	, _dropletImageId :: ImageId
	, _dropletRegionId :: RegionId
	, _dropletSizeId :: SizeOptionId
	, _dropletStatus :: T.Text
	} deriving (Read, Show, Eq)

data NewDropletOptions = NewDropletOptions
	{ _newdropletoptionsName :: T.Text
	, _newdropletoptionsSizeId :: SizeOptionId
	, _newdropletoptionsImageId :: ImageId
	, _newdropletoptionsRegionId :: RegionId
	, _newdropletoptionsSshKeyIds :: [SshKeyId]
	} deriving (Read, Show, Eq)

data NewDroplet = NewDroplet
	{ _newdropletId :: DropletId
	, _newdropletName :: T.Text
	, _newdropletImageId :: ImageId
	, _newdropletSizeId :: SizeOptionId
	, _newdropletEventId :: EventId
	} deriving (Read, Show, Eq)

data Event = Event
	{ _eventEventId :: EventId
	} deriving (Read, Show, Eq)

data Region = Region
	{ _regionId :: RegionId
	, _regionName :: T.Text
	} deriving (Read, Show, Eq)

data Image = Image
	{ _imageId :: ImageId
	, _imageName :: T.Text
	, _imageDistribution :: T.Text
	} deriving (Read, Show, Eq)

data SshKeyInfo = SshKeyInfo
	{ _sshkeyinfoId :: SshKeyId
	, _sshkeyinfoName :: T.Text
	} deriving (Read, Show, Eq)

data SshKey = SshKey
	{ _sshkeyId :: SshKeyId
	, _sshkeyName :: T.Text
	, _sshkeySshPubKey :: T.Text
	} deriving (Read, Show, Eq)

data SizeOption = SizeOption
	{ _sizeoptionId :: SizeOptionId
	, _sizeoptionName :: T.Text
	} deriving (Read, Show, Eq)

data Domain = Domain
	{ _domainId :: DomainId
	, _domainName :: T.Text
	, _domainTtl :: Int
	, _domainLiveZoneFile :: T.Text
	, _domainError :: Maybe T.Text
	, _domainZoneFileWithError :: Maybe T.Text
	} deriving (Read, Show, Eq)

data DomainRecord = DomainRecord
	{ _domainrecordId :: RecordId
	, _domainrecordDomainId :: DomainId
	, _domainrecordType :: T.Text
	, _domainrecordName :: T.Text
	, _domainrecordData :: T.Text
	, _domainrecordPriority :: Maybe Int
	, _domainrecordPort :: Maybe Int
	, _domainrecordWeight :: Maybe Int
	} deriving (Read, Show, Eq)

data NewRecord = NewRecord
	{ _newrecordRecordType :: T.Text
	, _newrecordInfo :: T.Text
	, _newrecordName :: Maybe T.Text
	, _newrecordPriority :: Maybe Int
	, _newrecordPort :: Maybe Int
	, _newrecordWeight :: Maybe Int
	} deriving (Read, Show, Eq)

makeFields ''Credentials
makeFields ''NewDropletOptions
makeFields ''Droplet
makeFields ''NewDroplet
makeFields ''Event
makeFields ''Region
makeFields ''Image
makeFields ''SshKeyInfo
makeFields ''SshKey
makeFields ''SizeOption
makeFields ''Domain
makeFields ''DomainRecord
makeFields ''NewRecord

deriveJSON Prelude.id ''Credentials
deriveJSON Prelude.id ''NewDropletOptions
deriveJSON Prelude.id ''Droplet
deriveJSON Prelude.id ''NewDroplet
deriveJSON Prelude.id ''Event
deriveJSON Prelude.id ''Region
deriveJSON Prelude.id ''Image
deriveJSON Prelude.id ''SshKeyInfo
deriveJSON Prelude.id ''SshKey
deriveJSON Prelude.id ''SizeOption
deriveJSON Prelude.id ''Domain
deriveJSON Prelude.id ''DomainRecord
deriveJSON Prelude.id ''NewRecord

data ImageType = MyImages | Global

instance ToTemplateValue T.Text SingleElement where
  toTemplateValue = Single . T.unpack

instance (ToTemplateValue a SingleElement) => ToTemplateValue (Maybe a) SingleElement where
  toTemplateValue Nothing = Single ""
  toTemplateValue (Just v) = toTemplateValue v

instance ToTemplateValue ImageType SingleElement where
	toTemplateValue MyImages = Single "my_images"
	toTemplateValue Global = Single "global"

instance ToTemplateValue [SshKeyId] SingleElement where
  toTemplateValue ks = Single $ intercalate "," $ map show ks

instance ToTemplateValue Credentials AssociativeListElement where
  toTemplateValue x = Associative
    [ ("api_key", toTemplateValue $ x ^. apiKey)
    , ("client_id", toTemplateValue $ x ^. clientId)
    ]

instance ToTemplateValue NewDropletOptions AssociativeListElement where
	toTemplateValue x = Associative
		[ ("name", toTemplateValue $ x ^. name)
		, ("size_id", toTemplateValue $ x ^. sizeId)
		, ("image_id", toTemplateValue $ x ^. imageId)
		, ("region_id", toTemplateValue $ x ^. regionId)
		, ("ssh_key_ids", toTemplateValue $ x ^. sshKeyIds)
		]

instance ToTemplateValue NewRecord AssociativeListElement where
	toTemplateValue x = Associative
		[ ("record_type", toTemplateValue $ x ^. recordType)
		, ("data", toTemplateValue $ x ^. info)
		, ("name", toTemplateValue $ x ^. name)
		, ("priority", toTemplateValue $ x ^. priority)
		, ("port", toTemplateValue $ x ^. port)
		, ("weight", toTemplateValue $ x ^. weight)
		]

newtype DigitalOcean a = DigitalOcean { fromDigitalOcean :: ReaderT Credentials API.APIClient a }
	deriving (Functor, Applicative, Monad, MonadIO)

runDigitalOcean :: Credentials -> DigitalOcean a -> IO (Either API.APIError a)
runDigitalOcean c m = API.runAPIClient
  "https://api.digitalocean.com/"
  Prelude.id
  (runReaderT (fromDigitalOcean m) c)

get :: FromJSON a => String -> DigitalOcean (Response a)
get = DigitalOcean . lift . API.get . pack

creds :: DigitalOcean Credentials
creds = DigitalOcean ask

-- | /droplets
getDroplets :: DigitalOcean (Response [Droplet])
getDroplets = do
  credentials <- creds
  get [uri| /droplets{?credentials*} |]

---- | /droplets/new
addDroplet :: NewDropletOptions -> DigitalOcean (Response NewDroplet)
addDroplet newDroplet = do
  credentials <- creds
  get [uri| /droplets/new{?newDroplet*, credentials*} |]

---- | /droplets/{dropletId}
getDroplet :: DropletId -> DigitalOcean (Response Droplet)
getDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}{?credentials*} |]

---- /droplets/{dropletId}/reboot
rebootDroplet :: DropletId -> DigitalOcean (Response Event)
rebootDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/reboot{?credentials*} |]

---- /droplets/{dropletId}/power_cycle
powerCycleDroplet :: DropletId -> DigitalOcean (Response Event)
powerCycleDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/power_cycle{?credentials*} |]

---- /droplets/{dropletId}/shutdown
shutdownDroplet :: DropletId -> DigitalOcean (Response Event)
shutdownDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/shutdown{?credentials*} |]

---- /droplets/{dropletId}/power_off
powerOffDroplet :: DropletId -> DigitalOcean (Response Event)
powerOffDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/power_off{?credentials*} |]

---- /droplets/{dropletId}/power_on
powerOnDroplet :: DropletId -> DigitalOcean (Response Event)
powerOnDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/power_on{?credentials*} |]

---- /droplets/{dropletId}/password_reset
resetRootDropletPassword :: DropletId -> DigitalOcean (Response Event)
resetRootDropletPassword dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/password_reset{?credentials*} |]

---- /droplets/{dropletId}/resize
resizeDroplet :: DropletId -> SizeOptionId -> DigitalOcean (Response Event)
resizeDroplet dropletId sizeId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/resize{?credentials*} |]
	where size_id = sizeId

---- /droplets/{dropletId}/snapshot
takeDropletSnapshot :: DropletId -> Maybe T.Text -> DigitalOcean (Response Event)
takeDropletSnapshot dropletId name = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/snapshot{?name, credentials*} |]

---- /droplets/{dropletId}/restore
restoreDroplet :: DropletId -> ImageId -> DigitalOcean (Response Event)
restoreDroplet dropletId imageId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/restore{?image_id, credentials*} |]
	where image_id = imageId

---- /droplets/{dropletId}/rebuild
rebuildDroplet :: DropletId -> ImageId -> DigitalOcean (Response Event)
rebuildDroplet dropletId imageId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/rebuild{?image_id, credentials*} |]
	where image_id = imageId

---- /droplets/{dropletId}/enable_backups
enableDropletBackups :: DropletId -> DigitalOcean (Response Event)
enableDropletBackups dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/enable_backups{?credentials*} |]

---- /droplets/{dropletId}/disable_backups
disableDropletBackups :: DropletId -> DigitalOcean (Response Event)
disableDropletBackups dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/disable_backups{?credentials*} |]

---- /droplets/{dropletId}/rename
renameDroplet :: DropletId -> String -> DigitalOcean (Response Event)
renameDroplet dropletId name = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/rename{?name, credentials*} |]

---- /droplets/{dropletId}/destroy
destroyDroplet :: DropletId -> DigitalOcean (Response Event)
destroyDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/destroy{?credentials*} |]

---- /regions
getRegions :: DigitalOcean (Response [Region])
getRegions = do
  credentials <- creds
  get [uri| /regions{?credentials*} |]

---- /images
getImages :: Maybe ImageType -> DigitalOcean (Response [Image])
getImages filter = do
  credentials <- creds
  get [uri| /images{?filter, credentials*} |]

---- /images/{imageId}
getImage :: ImageId -> DigitalOcean (Response Image)
getImage imageId = do
  credentials <- creds
  get [uri| /images/{imageId}{?credentials*} |]

---- /images/{imageId}/destroy
destroyImage :: ImageId -> DigitalOcean (Response ())
destroyImage imageId = do
  credentials <- creds
  get [uri| /images/{imageId}/destroy{?credentials*} |]

---- /images/{imageId}/transfer
transferImage :: ImageId -> RegionId -> DigitalOcean (Response Event)
transferImage imageId regionId = do
  credentials <- creds
  get [uri| /images/{imageId}/transfer{?region_id, credentials*} |]
	where region_id = regionId

---- /ssh_keys
getSshKeys :: DigitalOcean (Response [SshKeyInfo])
getSshKeys = do
  credentials <- creds
  get [uri| /ssh_keys{?credentials*} |]

---- /ssh_keys/new
addSshKey :: String -> String -> DigitalOcean (Response NewSshKey)
addSshKey name sshKeyPub = do
  credentials <- creds
  get [uri| /ssh_keys/new{?name, ssh_key_pub, credentials*} |]
	where ssh_key_pub = sshKeyPub

---- /ssh_key/{sshKeyId}
getSshKey :: SshKeyId -> DigitalOcean (Response SshKey)
getSshKey sshKeyId = do
  credentials <- creds
  get [uri| /ssh_key/{sshKeyId}{?credentials*} |]

---- /ssh_key/{sshKeyId}/edit
editSshKey :: SshKeyId -> String -> DigitalOcean (Response SshKey)
editSshKey sshKeyId sshKeyPub = do
  credentials <- creds
  get [uri| /ssh_key/{sshKeyId}/edit{?ssh_key_pub, credentials*} |]
	where ssh_key_pub = sshKeyPub

---- /ssh_key/{sshKeyId}/destroy
removeSshKey :: SshKeyId -> DigitalOcean (Response Bool)
removeSshKey sshKeyId = do
  credentials <- creds
  get [uri| /ssh_key/{sshKeyId}/destroy{?credentials*} |]

---- /sizes
getInstanceSizeOptions :: DigitalOcean (Response [SizeOption])
getInstanceSizeOptions = do
  credentials <- creds
  get [uri| /sizes{?credentials*} |]

---- /domains
getDomains :: DigitalOcean (Response [Domain])
getDomains = do
  credentials <- creds
  get [uri| /domains{?credentials*} |]

---- /domains/new
addDomain :: T.Text -> IpAddress -> DigitalOcean (Response NewDomain)
addDomain name ipAddress = do
  credentials <- creds
  get [uri| /domains/new{?name, ip_address, credentials*} |]
	where ip_address = ipAddress

---- /domains/{domainId}
getDomain :: DomainId -> DigitalOcean (Response Domain)
getDomain domainId = do
  credentials <- creds
  get [uri| /domains/{domainId}{?credentials*} |]

---- /domains/{domainId}/destroy
removeDomain :: DomainId -> DigitalOcean (Response Bool)
removeDomain domainId = do
  credentials <- creds
  get [uri| /domains/{domainId}/destroy{?credentials*} |]

---- /domains/{domainId}/records
getDomainRecords :: DomainId -> DigitalOcean (Response [DomainRecord])
getDomainRecords domainId = do
  credentials <- creds
  get [uri| /domains/{domainId}/records{?credentials*} |]

---- /domains/{domainId}/records/new
addDomainRecord :: DomainId -> NewRecord -> DigitalOcean (Response DomainRecord)
addDomainRecord domainId newRecord = do
  credentials <- creds
  get [uri| /domains/{domainId}/records/new{?newRecord*, credentials*} |]

---- /domains/{domainId}/records/{recordId}
getDomainRecord :: DomainId -> RecordId -> DigitalOcean (Response DomainRecord)
getDomainRecord domainId recordId = do
  credentials <- creds
  get [uri| /domains/{domainId}/records/{recordId}{?credentials*} |]

---- /domains/{domainId}/records/{recordId}/destroy
removeDomainRecord :: DomainId -> RecordId -> DigitalOcean (Response Bool)
removeDomainRecord domainId recordId = do
  credentials <- creds
  get [uri| /domains/{domainId}/records/{recordId}/destroy{?credentials*} |]


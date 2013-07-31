{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Network.DigitalOcean where
import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.List
import qualified Network.API as API

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

data ImageType = MyImages | Global

instance API.ParameterEncodable ImageType where
	paramEncode MyImages = Just "my_images"
	paramEncode Global = Just "global"

instance API.ToRouteParameters NewDropletOptions where
	toParams x =
		[ ("name", p x name)
		, ("size_id", p x sizeId)
		, ("image_id", p x imageId)
		, ("region_id", p x regionId)
		, ("ssh_key_ids", p x sshKeyIds)
		]

instance API.ToRouteParameters NewRecord where
	toParams x =
		[ ("record_type", p x recordType)
		, ("data", p x info)
		, ("name", p x name)
		, ("priority", p x priority)
		, ("port", p x port)
		, ("weight", p x weight)
		]

newtype DigitalOcean a = DigitalOcean { fromDigitalOcean :: APIClient a }
	deriving (Functor, Applicative, Monad, MonadIO)

runDigitalOcean :: Credentials -> DigitalOcean a -> IO (Either APIError a)
runDigitalOcean c m = runAPIClient
  "https://api.digitalocean.com/"
  id
  (runReaderT (fromDigitalOcean m) c)

get :: FromJSON a => String -> DigitalOcean a
get = DigitalOcean . lift . API.get

creds :: DigitalOcean Credentials
creds = DigitalOcean ask

-- | /droplets
getDroplets :: DigitalOcean [Droplet]
getDroplets = get [uri| /droplets{?credentials?} |]

---- | /droplets/new
addDroplet :: NewDropletOptions -> DigitalOcean NewDroplet
addDroplet newDroplet = do
  credentials <- creds
  get [uri| /droplets/new{?newDroplet*, credentials*} |]

---- | /droplets/{dropletId}
getDroplet :: DropletId -> DigitalOcean (Maybe Droplet)
getDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}{?credentials*} |]

---- /droplets/{dropletId}/reboot
rebootDroplet :: DropletId -> DigitalOcean (Maybe Event)
rebootDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/reboot{?credentials*} |]

---- /droplets/{dropletId}/power_cycle
powerCycleDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerCycleDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/power_cycle{?credentials*} |]

---- /droplets/{dropletId}/shutdown
shutdownDroplet :: DropletId -> DigitalOcean (Maybe Event)
shutdownDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/shutdown{?credentials*} |]

---- /droplets/{dropletId}/power_off
powerOffDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerOffDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/power_off{?credentials*} |]

---- /droplets/{dropletId}/power_on
powerOnDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerOnDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/power_on{?credentials*} |]

---- /droplets/{dropletId}/password_reset
resetRootDropletPassword :: DropletId -> DigitalOcean (Maybe Event)
resetRootDropletPassword dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/password_reset{?credentials*} |]

---- /droplets/{dropletId}/resize
resizeDroplet :: DropletId -> SizeOptionId -> DigitalOcean (Maybe Event)
resizeDroplet dropletId sizeId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/resize{?credentials*} |]
	where size_id = sizeId

---- /droplets/{dropletId}/snapshot
takeDropletSnapshot :: DropletId -> Maybe T.Text -> DigitalOcean (Maybe Event)
takeDropletSnapshot dropletId name = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/snapshot{?name, credentials*} |]

---- /droplets/{dropletId}/restore
restoreDroplet :: DropletId -> ImageId -> DigitalOcean (Maybe Event)
restoreDroplet dropletId imageId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/restore{?image_id, credentials*} |]
	where image_id = imageId

---- /droplets/{dropletId}/rebuild
rebuildDroplet :: DropletId -> ImageId -> DigitalOcean (Maybe Event)
rebuildDroplet dropletId imageId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/rebuild{?image_id, credentials*} |]
	where image_id = imageId

---- /droplets/{dropletId}/enable_backups
enableDropletBackups :: DropletId -> DigitalOcean (Maybe Event)
enableDropletBackups dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/enable_backups{?credentials*} |]

---- /droplets/{dropletId}/disable_backups
disableDropletBackups :: DropletId -> DigitalOcean (Maybe Event)
disableDropletBackups dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/disable_backups{?credentials*} |]

---- /droplets/{dropletId}/rename
renameDroplet :: DropletId -> String -> DigitalOcean (Maybe Event)
renameDroplet dropletId name = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/rename{?name, credentials*} |]

---- /droplets/{dropletId}/destroy
destroyDroplet :: DropletId -> DigitalOcean (Maybe Event)
destroyDroplet dropletId = do
  credentials <- creds
  get [uri| /droplets/{dropletId}/destroy{?credentials*} |]

---- /regions
getRegions :: DigitalOcean [Region]
getRegions = do
  credentials <- creds
  get [uri| /regions{?credentials*} |]

---- /images
getImages :: Maybe ImageType -> DigitalOcean [Image]
getImages filter = do
  credentials <- creds
  get [uri| /images{?filter, credentials*} |]

---- /images/{imageId}
getImage :: ImageId -> DigitalOcean (Maybe Image)
getImage imageId = do
  credentials <- creds
  get [uri| /images/{imageId}{?credentials*} |]

---- /images/{imageId}/destroy
destroyImage :: ImageId -> DigitalOcean ()
destroyImage imageId = do
  credentials <- creds
  get [uri| /images/{imageId}/destroy{?credentials*} |]

---- /images/{imageId}/transfer
transferImage :: ImageId -> RegionId -> DigitalOcean (Maybe Event)
transferImage imageId regionId = do
  credentials <- creds
  get [uri| /images/{imageId}/transfer{?region_id, credentials*} |]
	where region_id = regionId

---- /ssh_keys
getSshKeys :: DigitalOcean [SshKeyInfo]
getSshKeys = do
  credentials <- creds
  get [uri| /ssh_keys{?credentials*} |]

---- /ssh_keys/new
addSshKey :: String -> String -> DigitalOcean NewSshKey
addSshKey name sshKeyPub = do
  credentials <- creds
  get [uri| /ssh_keys/new{?name, ssh_key_pub, credentials*} |]
	where ssh_key_pub = sshKeyPub

---- /ssh_key/{sshKeyId}
getSshKey :: SshKeyId -> DigitalOcean (Maybe SshKey)
getSshKey sshKetId = do
  credentials <- creds
  get [uri| /ssh_key/{sshKeyId}{?credentials*} |]

---- /ssh_key/{sshKeyId}/edit
editSshKey :: SshKeyId -> String -> DigitalOcean SshKey
editSshKey sshIdKey sshKeyPub = do
  credentials <- creds
  get [uri| /ssh_key/{sshKeyId}/edit{?ssh_key_pub, credentials*} |]
	where ssh_key_pub = sshKeyPub

---- /ssh_key/{sshKeyId}/destroy
removeSshKey :: SshKeyId -> DigitalOcean Bool
removeSshKey sshKeyId = do
  credentials <- creds
  get [uri| /ssh_key/{sshKeyId}/destroy{?credentials*} |]

---- /sizes
getInstanceSizeOptions :: DigitalOcean [SizeOption]
getInstanceSizeOptions = do
  credentials <- creds
  get [uri| /sizes{?credentials*} |]

---- /domains
getDomains :: DigitalOcean [Domain]
getDomains = do
  credentials <- creds
  get [uri| /domains{?credentials*} |]

---- /domains/new
addDomain :: T.Text -> IpAddress -> DigitalOcean NewDomain
addDomain name ipAddress = do
  credentials <- creds
  get [uri| /domains/new{?name, ip_address, credentials*} |]
	where ip_address = ipAddress

---- /domains/{domainId}
getDomain :: DomainId -> DigitalOcean (Maybe Domain)
getDomain domainId = do
  credentials <- creds
  get [uri| /domains/{domainId}{?credentials*} |]

---- /domains/{domainId}/destroy
removeDomain :: DomainId -> DigitalOcean Bool
removeDomain domainId = do
  credentials <- creds
  get [uri| /domains/{domainId}/destroy{?credentials*} |]

---- /domains/{domainId}/records
getDomainRecords :: DomainId -> DigitalOcean [DomainRecord]
getDomainRecords domainId = do
  credentials <- creds
  get [uri| /domains/{domainId}/records{?credentials*} |]

---- /domains/{domainId}/records/new
addDomainRecord :: DomainId -> NewRecord -> DigitalOcean DomainRecord
addDomainRecord domainId newRecord = do
  credentials <- creds
  get [uri| /domains/{domainId}/records/new{?newRecord*, credentials*} |]

---- /domains/{domainId}/records/{recordId}
getDomainRecord :: DomainId -> RecordId -> DigitalOcean (Maybe DomainRecord)
getDomainRecord domainId recordId = do
  credentials <- creds
  get [uri| /domains/{domainId}/records/{recordId}{?credentials*} |]

---- /domains/{domainId}/records/{recordId}/destroy
removeDomainRecord :: DomainId -> RecordId -> DigitalOcean Bool
removeDomainRecord domainId recordId = do
  credentials <- creds
  get [uri| /domains/{domainId}/records/{recordId}/destroy{?credentials*} |]


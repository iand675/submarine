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
  (\r -> r { path = path r ++ "&client_id=" ++ c })
  (fromDigitalOcean m)

-- get :: T.Text -> [(ByteString, Maybe ByteString)] -> DigitalOcean a
-- get t rs = get $ undefined t rs

-- | /droplets
getDroplets :: DigitalOcean [Droplet]
getDroplets = DigitalOcean $ get [url| /droplets |]

---- | /droplets/new
addDroplet :: NewDropletOptions -> DigitalOcean NewDroplet
addDroplet newDroplet = DigitalOcean $ get [url| /droplets/new{?newDroplet*} |]

---- | /droplets/{dropletId}
getDroplet :: DropletId -> DigitalOcean (Maybe Droplet)
getDroplet dropletId = DigitalOcean $ get [url| /droplets/{dropletId} |]

---- /droplets/{dropletId}/reboot
rebootDroplet :: DropletId -> DigitalOcean (Maybe Event)
rebootDroplet dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/reboot |]

---- /droplets/{dropletId}/power_cycle
powerCycleDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerCycleDroplet dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/power_cycle |]

---- /droplets/{dropletId}/shutdown
shutdownDroplet :: DropletId -> DigitalOcean (Maybe Event)
shutdownDroplet dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/shutdown|]

---- /droplets/{dropletId}/power_off
powerOffDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerOffDroplet dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/power_off|]

---- /droplets/{dropletId}/power_on
powerOnDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerOnDroplet dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/power_on|]

---- /droplets/{dropletId}/password_reset
resetRootDropletPassword :: DropletId -> DigitalOcean (Maybe Event)
resetRootDropletPassword dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/password_reset|]

---- /droplets/{dropletId}/resize
resizeDroplet :: DropletId -> SizeOptionId -> DigitalOcean (Maybe Event)
resizeDroplet dropletId sizeId = DigitalOcean $ get [url| /droplets/{dropletId}/resize|]
	where size_id = sizeId

---- /droplets/{dropletId}/snapshot
takeDropletSnapshot :: DropletId -> Maybe T.Text -> DigitalOcean (Maybe Event)
takeDropletSnapshot dropletId name = DigitalOcean $ get [url| /droplets/{dropletId}/snapshot{?name} |]

---- /droplets/{dropletId}/restore
restoreDroplet :: DropletId -> ImageId -> DigitalOcean (Maybe Event)
restoreDroplet dropletId imageId = DigitalOcean $ get [url| /droplets/{dropletId}/restore{?image_id} |]
	where image_id = imageId

---- /droplets/{dropletId}/rebuild
rebuildDroplet :: DropletId -> ImageId -> DigitalOcean (Maybe Event)
rebuildDroplet dropletId imageId = DigitalOcean $ get [url| /droplets/{dropletId}/rebuild{?image_id} |]
	where image_id = imageId

---- /droplets/{dropletId}/enable_backups
enableDropletBackups :: DropletId -> DigitalOcean (Maybe Event)
enableDropletBackups dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/enable_backups |]

---- /droplets/{dropletId}/disable_backups
disableDropletBackups :: DropletId -> DigitalOcean (Maybe Event)
disableDropletBackups dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/disable_backups |]

---- /droplets/{dropletId}/rename
renameDroplet :: DropletId -> String -> DigitalOcean (Maybe Event)
renameDroplet dropletId name = DigitalOcean $ get [url| /droplets/{dropletId}/rename{?name} |]

---- /droplets/{dropletId}/destroy
destroyDroplet :: DropletId -> DigitalOcean (Maybe Event)
destroyDroplet dropletId = DigitalOcean $ get [url| /droplets/{dropletId}/destroy |]

---- /regions
getRegions :: DigitalOcean [Region]
getRegions = DigitalOcean $ get [url| /regions |]

---- /images
getImages :: Maybe ImageType -> DigitalOcean [Image]
getImages filter = DigitalOcean $ get [url| /images{?filter} |]

---- /images/{imageId}
getImage :: ImageId -> DigitalOcean (Maybe Image)
getImage imageId = DigitalOcean $ get [url| /images/{imageId} |]

---- /images/{imageId}/destroy
destroyImage :: ImageId -> DigitalOcean ()
destroyImage imageId = DigitalOcean $ get [url| /images/{imageId}/destroy |]

---- /images/{imageId}/transfer
transferImage :: ImageId -> RegionId -> DigitalOcean (Maybe Event)
transferImage imageId regionId = DigitalOcean $ get [url| /images/{imageId}/transfer{?region_id} |]
	where region_id = regionId

---- /ssh_keys
getSshKeys :: DigitalOcean [SshKeyInfo]
getSshKeys = DigitalOcean $ get [url| /ssh_keys |]

---- /ssh_keys/new
addSshKey :: String -> String -> DigitalOcean NewSshKey
addSshKey name sshKeyPub = DigitalOcean $ get [url| /ssh_keys/new{?name, ssh_key_pub} |]
	where ssh_key_pub = sshKeyPub

---- /ssh_key/{sshKeyId}
getSshKey :: SshKeyId -> DigitalOcean (Maybe SshKey)
getSshKey sshKetId = DigitalOcean $ get [url| /ssh_key/{sshKeyId} |]

---- /ssh_key/{sshKeyId}/edit
editSshKey :: SshKeyId -> String -> DigitalOcean SshKey
editSshKey sshIdKey sshKeyPub = DigitalOcean $ get [url| /ssh_key/{sshKeyId}/edit{?ssh_key_pub} |]
	where ssh_key_pub = sshKeyPub

---- /ssh_key/{sshKeyId}/destroy
removeSshKey :: SshKeyId -> DigitalOcean Bool
removeSshKey sshKeyId = DigitalOcean $ get [url| /ssh_key/{sshKeyId}/destroy |]

---- /sizes
getInstanceSizeOptions :: DigitalOcean [SizeOption]
getInstanceSizeOptions = DigitalOcean $ get [url| /sizes |]

---- /domains
getDomains :: DigitalOcean [Domain]
getDomains = DigitalOcean $ get [url| /domains |]

---- /domains/new
addDomain :: T.Text -> IpAddress -> DigitalOcean NewDomain
addDomain name ipAddress = DigitalOcean $ get [url| /domains/new{?name, ip_address} |]
	where ip_address = ipAddress

---- /domains/{domainId}
getDomain :: DomainId -> DigitalOcean (Maybe Domain)
getDomain domainId = DigitalOcean $ get [url| /domains/{domainId} |]

---- /domains/{domainId}/destroy
removeDomain :: DomainId -> DigitalOcean Bool
removeDomain domainId = DigitalOcean $ get [url| /domains/{domainId}/destroy |]

---- /domains/{domainId}/records
getDomainRecords :: DomainId -> DigitalOcean [DomainRecord]
getDomainRecords domainId = DigitalOcean $ get [url| /domains/{domainId}/records |]

---- /domains/{domainId}/records/new
addDomainRecord :: DomainId -> NewRecord -> DigitalOcean DomainRecord
addDomainRecord domainId newRecord = DigitalOcean $ get [url| /domains/{domainId}/records/new{?newRecord*} |]

---- /domains/{domainId}/records/{recordId}
getDomainRecord :: DomainId -> RecordId -> DigitalOcean (Maybe DomainRecord)
getDomainRecord domainId recordId = DigitalOcean $ get [url| /domains/{domainId}/records/{recordId} |]

---- /domains/{domainId}/records/{recordId}/destroy
removeDomainRecord :: DomainId -> RecordId -> DigitalOcean Bool
removeDomainRecord domainId recordId = DigitalOcean $ get [url| /domains/{domainId}/records/{recordId}/destroy |]


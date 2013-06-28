{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
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
import Network.HTTP.Conduit hiding (port)

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

newtype DigitalOcean a = DigitalOcean { runDigitalOcean :: ReaderT Credentials APIClient a }
	deriving (Monad, MonadIO, Functor)

-- get :: T.Text -> [(ByteString, Maybe ByteString)] -> DigitalOcean a
-- get t rs = get $ undefined t rs

-- | /droplets
getDroplets :: DigitalOcean [Droplet]
getDroplets = get [url| /droplets |]

---- | /droplets/new
addDroplet :: NewDropletOptions -> DigitalOcean NewDroplet
addDroplet newDroplet = get [url| /droplets/new{?newDroplet*} |]

---- | /droplets/{dropletId}
getDroplet :: DropletId -> DigitalOcean (Maybe Droplet)
getDroplet dropletId = get [url| /droplets/{dropletId} |]

---- /droplets/{dropletId}/reboot
rebootDroplet :: DropletId -> DigitalOcean (Maybe Event)
rebootDroplet dropletId = get [url| /droplets/{dropletId}/reboot |]

---- /droplets/{dropletId}/power_cycle
powerCycleDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerCycleDroplet dropletId = get [url| /droplets/{dropletId}/power_cycle |]

---- /droplets/{dropletId}/shutdown
shutdownDroplet :: DropletId -> DigitalOcean (Maybe Event)
shutdownDroplet dropletId = get [url| /droplets/{dropletId}/shutdown|]

---- /droplets/{dropletId}/power_off
powerOffDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerOffDroplet dropletId = get [url| /droplets/{dropletId}/power_off|]

---- /droplets/{dropletId}/power_on
powerOnDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerOnDroplet dropletId = get [url| /droplets/{dropletId}/power_on|]

---- /droplets/{dropletId}/password_reset
resetRootDropletPassword :: DropletId -> DigitalOcean (Maybe Event)
resetRootDropletPassword dropletId = get [url| /droplets/{dropletId}/password_reset|]

---- /droplets/{dropletId}/resize
resizeDroplet :: DropletId -> SizeOptionId -> DigitalOcean (Maybe Event)
resizeDroplet dropletId sizeId = get [url| /droplets/{dropletId}/resize|]
	where size_id = sizeId

---- /droplets/{dropletId}/snapshot
takeDropletSnapshot :: DropletId -> Maybe T.Text -> DigitalOcean (Maybe Event)
takeDropletSnapshot dropletId name = get [url| /droplets/{dropletId}/snapshot{?name} |]

---- /droplets/{dropletId}/restore
restoreDroplet :: DropletId -> ImageId -> DigitalOcean (Maybe Event)
restoreDroplet dropletId imageId = get [url| /droplets/{dropletId}/restore{?image_id} |]
	where image_id = imageId

---- /droplets/{dropletId}/rebuild
rebuildDroplet :: DropletId -> ImageId -> DigitalOcean (Maybe Event)
rebuildDroplet dropletId imageId = get [url| /droplets/{dropletId}/rebuild{?image_id} |]
	where image_id = imageId

---- /droplets/{dropletId}/enable_backups
enableDropletBackups :: DropletId -> DigitalOcean (Maybe Event)
enableDropletBackups dropletId = get [url| /droplets/{dropletId}/enable_backups |]

---- /droplets/{dropletId}/disable_backups
disableDropletBackups :: DropletId -> DigitalOcean (Maybe Event)
disableDropletBackups dropletId = get [url| /droplets/{dropletId}/disable_backups |]

---- /droplets/{dropletId}/rename
renameDroplet :: DropletId -> String -> DigitalOcean (Maybe Event)
renameDroplet dropletId name = get [url| /droplets/{dropletId}/rename{?name} |]

---- /droplets/{dropletId}/destroy
destroyDroplet :: DropletId -> DigitalOcean (Maybe Event)
destroyDroplet dropletId = get [url| /droplets/{dropletId}/destroy |]

---- /regions
getRegions :: DigitalOcean [Region]
getRegions = get [url| /regions |]

---- /images
getImages :: Maybe ImageType -> DigitalOcean [Image]
getImages filter = get [url| /images{?filter} |]

---- /images/{imageId}
getImage :: ImageId -> DigitalOcean (Maybe Image)
getImage imageId = get [url| /images/{imageId} |]

---- /images/{imageId}/destroy
destroyImage :: ImageId -> DigitalOcean ()
destroyImage imageId = get [url| /images/{imageId}/destroy |]

---- /images/{imageId}/transfer
transferImage :: ImageId -> RegionId -> DigitalOcean (Maybe Event)
transferImage imageId regionId = get [url| /images/{imageId}/transfer{?region_id} |]
	where region_id = regionId

---- /ssh_keys
getSshKeys :: DigitalOcean [SshKeyInfo]
getSshKeys = get [url| /ssh_keys |]

---- /ssh_keys/new
addSshKey :: String -> String -> DigitalOcean NewSshKey
addSshKey name sshKeyPub = get [url| /ssh_keys/new{?name, ssh_key_pub} |]
	where ssh_key_pub = sshKeyPub

---- /ssh_key/{sshKeyId}
getSshKey :: SshKeyId -> DigitalOcean (Maybe SshKey)
getSshKey sshKetId = get [url| /ssh_key/{sshKeyId} |]

---- /ssh_key/{sshKeyId}/edit
editSshKey :: SshKeyId -> String -> DigitalOcean SshKey
editSshKey sshIdKey sshKeyPub = get [url| /ssh_key/{sshKeyId}/edit{?ssh_key_pub} |]
	where ssh_key_pub = sshKeyPub

---- /ssh_key/{sshKeyId}/destroy
removeSshKey :: SshKeyId -> DigitalOcean Bool
removeSshKey sshKeyId = get [url| /ssh_key/{sshKeyId}/destroy |]

---- /sizes
getInstanceSizeOptions :: DigitalOcean [SizeOption]
getInstanceSizeOptions = get [url| /sizes |]

---- /domains
getDomains :: DigitalOcean [Domain]
getDomains = get [url| /domains |]

---- /domains/new
addDomain :: T.Text -> IpAddress -> DigitalOcean NewDomain
addDomain name ipAddress = get [url| /domains/new{?name, ip_address} |]
	where ip_address = ipAddress

---- /domains/{domainId}
getDomain :: DomainId -> DigitalOcean (Maybe Domain)
getDomain domainId = get [url| /domains/{domainId} |]

---- /domains/{domainId}/destroy
removeDomain :: DomainId -> DigitalOcean Bool
removeDomain domainId = get [url| /domains/{domainId}/destroy |]

---- /domains/{domainId}/records
getDomainRecords :: DomainId -> DigitalOcean [DomainRecord]
getDomainRecords domainId = get [url| /domains/{domainId}/records |]

---- /domains/{domainId}/records/new
addDomainRecord :: DomainId -> NewRecord -> DigitalOcean DomainRecord
addDomainRecord domainId newRecord = get [url| /domains/{domainId}/records/new{?newRecord*} |]

---- /domains/{domainId}/records/{recordId}
getDomainRecord :: DomainId -> RecordId -> DigitalOcean (Maybe DomainRecord)
getDomainRecord domainId recordId = get [url| /domains/{domainId}/records/{recordId} |]

---- /domains/{domainId}/records/{recordId}/destroy
removeDomainRecord :: DomainId -> RecordId -> DigitalOcean Bool
removeDomainRecord domainId recordId = get [url| /domains/{domainId}/records/{recordId}/destroy |]

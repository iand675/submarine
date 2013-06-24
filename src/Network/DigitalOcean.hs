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
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
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

e :: API.ParameterEncodable a => a -> Maybe ByteString
e = API.paramEncode
p x n = e (x ^. n)

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

newtype DigitalOcean a = DigitalOcean { runDigitalOcean :: ReaderT Credentials (ResourceT IO) a }
	deriving (Monad, MonadIO, Functor)

replaceSection :: T.Text -> ByteString -> ByteString -> T.Text
replaceSection text placeholder val = T.replace (T.cons ':' $ decodeUtf8 placeholder) (decodeUtf8 val) text

get :: T.Text -> [(ByteString, Maybe ByteString)] -> DigitalOcean a
get = undefined
get' = undefined

-- | /droplets
getDroplets :: DigitalOcean [Droplet]
getDroplets = get' "/droplets"

---- | /droplets/new
addDroplet :: NewDropletOptions -> DigitalOcean NewDroplet
addDroplet = get "/droplets/new" . API.toParams

---- | /droplets/:id
getDroplet :: DropletId -> DigitalOcean (Maybe Droplet)
getDroplet i = get "/droplets/:id" [("id", e i)]

---- /droplets/:id/reboot
rebootDroplet :: DropletId -> DigitalOcean (Maybe Event)
rebootDroplet i = get "/droplets/:id/reboot" [("id", e i)]

---- /droplets/:id/power_cycle
powerCycleDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerCycleDroplet i = get "/droplets/:id/power_cycle" [("id", e i)]

---- /droplets/:id/shutdown
shutdownDroplet :: DropletId -> DigitalOcean (Maybe Event)
shutdownDroplet i = get "/droplets/:id/shutdown" [("id", e i)]

---- /droplets/:id/power_off
powerOffDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerOffDroplet i = get "/droplets/:id/power_off" [("id", e i)]

---- /droplets/:id/power_on
powerOnDroplet :: DropletId -> DigitalOcean (Maybe Event)
powerOnDroplet i = get "/droplets/:id/power_on" [("id", e i)]

---- /droplets/:id/password_reset
resetRootDropletPassword :: DropletId -> DigitalOcean (Maybe Event)
resetRootDropletPassword i = get "/droplets/:id/password_reset" [("id", e i)]

---- /droplets/:id/resize
resizeDroplet :: DropletId -> SizeOptionId -> DigitalOcean (Maybe Event)
resizeDroplet i s = get "/droplets/:id/resize" [("id", e i), ("size_id", e s)]

---- /droplets/:id/snapshot
takeDropletSnapshot :: DropletId -> Maybe T.Text -> DigitalOcean (Maybe Event)
takeDropletSnapshot i n = get "/droplets/:id/snapshot" [("id", e i), ("name", e n)]

---- /droplets/:id/restore
restoreDroplet :: DropletId -> ImageId -> DigitalOcean (Maybe Event)
restoreDroplet d i = get "/droplets/:id/restore" [("id", e d), ("image_id", e i)]

---- /droplets/:id/rebuild
rebuildDroplet :: DropletId -> ImageId -> DigitalOcean (Maybe Event)
rebuildDroplet d i = get "/droplets/:id/rebuild" [("id", e d), ("image_id", e i)]

---- /droplets/:id/enable_backups
enableDropletBackups :: DropletId -> DigitalOcean (Maybe Event)
enableDropletBackups i = get "/droplets/:id/enable_backups" [("id", e i)]

---- /droplets/:id/disable_backups
disableDropletBackups :: DropletId -> DigitalOcean (Maybe Event)
disableDropletBackups i = get "/droplets/:id/disable_backups" [("id", e i)]

---- /droplets/:id/rename
renameDroplet :: DropletId -> T.Text -> DigitalOcean (Maybe Event)
renameDroplet i n = get "/droplets/:id/rename" [("id", e i), ("name", e n)]

---- /droplets/:id/destroy
destroyDroplet :: DropletId -> DigitalOcean (Maybe Event)
destroyDroplet i = get "/droplets/:id/destroy" [("id", e i)]

---- /regions
getRegions :: DigitalOcean [Region]
getRegions = get' "/regions"

---- /images
getImages :: Maybe ImageType -> DigitalOcean [Image]
getImages f = get "/images" [("filter", e f)]

---- /images/:id
getImage :: ImageId -> DigitalOcean (Maybe Image)
getImage i = get "/images/:id" [("id", e i)]

---- /images/:id/destroy
destroyImage :: ImageId -> DigitalOcean ()
destroyImage i = get "/images/:id/destroy" [("id", e i)]

---- /images/:id/transfer
transferImage :: ImageId -> RegionId -> DigitalOcean (Maybe Event)
transferImage i r = get "/images/:id/transfer" [("id", e i)]

---- /ssh_keys
getSshKeys :: DigitalOcean [SshKeyInfo]
getSshKeys = get' "/ssh_keys"

---- /ssh_keys/new
addSshKey :: T.Text -> T.Text -> DigitalOcean NewSshKey
addSshKey n k = get "/ssh_keys/new" [("name", e n), ("ssh_key_pub", e k)]

---- /ssh_key/:id
getSshKey :: SshKeyId -> DigitalOcean (Maybe SshKey)
getSshKey i = get "/ssh_key/:id" []

---- /ssh_key/:id/edit
editSshKey :: SshKeyId -> T.Text -> DigitalOcean SshKey
editSshKey i t = get "/ssh_key/:id/edit" [("id", e i)]

---- /ssh_key/:id/destroy
removeSshKey :: SshKeyId -> DigitalOcean Bool
removeSshKey i = get "/ssh_key/:id/destroy" [("id", e i)]

---- /sizes
getInstanceSizeOptions :: DigitalOcean [SizeOption]
getInstanceSizeOptions = get' "/sizes"

---- /domains
getDomains :: DigitalOcean [Domain]
getDomains = get' "/domains"

---- /domains/new
addDomain :: T.Text -> IpAddress -> DigitalOcean NewDomain
addDomain t i = get "/domains/new" [("name", e t), ("ip_address", e i)]

---- /domains/:id
getDomain :: DomainId -> DigitalOcean (Maybe Domain)
getDomain i = get "/domains/:id" [("id", e i)]

---- /domains/:id/destroy
removeDomain :: DomainId -> DigitalOcean Bool
removeDomain i = get "/domains/:id/destroy" [("id", e i)]

---- /domains/:id/records
getDomainRecords :: DomainId -> DigitalOcean [DomainRecord]
getDomainRecords i = get "/domains/:id/records" [("id", e i)]

---- /domains/:id/records/new
addDomainRecord :: DomainId -> NewRecord -> DigitalOcean DomainRecord
addDomainRecord i r = get "/domains/:id/records/new" $ [("id", e i)] ++ API.toParams r

---- /domains/:id/records/:id
getDomainRecord :: DomainId -> RecordId -> DigitalOcean (Maybe DomainRecord)
getDomainRecord d r = get "/domains/:id/records/:id" [("domain_id", e d), ("record_id", e r)]

---- /domains/:id/records/:id/destroy
removeDomainRecord :: DomainId -> RecordId -> DigitalOcean Bool
removeDomainRecord d r = get "/domains/:id/records/:id/destroy" [("domain_id", e d), ("record_id", e r)]

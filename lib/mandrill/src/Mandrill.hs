module Network.Mandrill where

newtype Mandrill a = Mandrill
	{ fromMandrill :: ReaderT MandrillConfig (ResourceT IO) a
	} deriving (Functor, Applicative, Monad)

runMandrill 

data EmailStatus = Sent | Queued | Scheduled | Rejected | Invalid

data WebHookEventType
	= Send
	| HardBounce
	| SoftBounce
	| Open
		{ ip :: Text
		, userAgent :: Text
		, location :: Location
		, parsedUserAgent :: ParsedUserAgent
		}
	| Click
		{ url :: Text
		, ip :: Text
		, userAgent :: Text
		, location :: Location
		, parsedUserAgent :: ParsedUserAgent
		}
	| Spam
	| Unsubscribe
	| Reject

data WebHookEvent = WebHookEvent
	{ timestamp :: Int
	, eventType :: WebHookEventType
	, message :: MessageDetails
	}

data User = User
	{ _userUsername :: Text
	, _userCreatedAt :: UTCTime
	, _userPublicId :: Text
	, _userReputation :: Int
	, _userHourlyQuota :: Int
	, _userBacklog :: Int
	, _userStats :: UserStats
	}

data UserStats = UserStats
	{ _userstatsToday :: UserStat
	, _userstatsLast7Days :: UserStat
	, _userstatsLast30Days :: UserStat
	, _userstatsLast60Days :: UserStat
	, _userstatsLast90Days :: UserStat
	, _userstatsAllTime :: UserStat
	}

data UserStat = UserStat
	{ _userstatSent :: Int
	, _userstatHardBounces :: Int
	, _userstatSoftBounces :: Int
	, _userstatRejects :: Int
	, _userstatComplaints :: Int
	, _userstatUnsubs :: Int
	, _userstatOpens :: Int
	, _userstatUniqueOpens :: Int
	, _userstatClicks :: Int
	, _userstatUniqueClicks :: Int
	}

-- /users/info.json
getUserInfo :: Mandrill User
getUserInfo = post [url| /users/info.json |]

data Pong = Pong { ping :: Text }

-- /users/ping2.json
ping :: Mandrill Pong
ping = post [url| /users/ping2.json |]

data Sender = Sender
	{ _senderAddress :: Text
	, _senderCreatedAt :: UTCTime
	, _senderSent :: Int
	, _senderHardBounces :: Int
	, _senderSoftBounces :: Int
	, _senderRejects :: Int
	, _senderComplaints :: Int
	, _senderUnsubs :: Int
	, _senderOpens :: Int
	, _senderClicks :: Int
	, _senderUniqueOpens :: Int
	, _senderUniqueClicks :: Int
	}

-- /users/senders.json
listUserSenders :: Mandrill [Sender]
listUserSenders = post [url| /users/senders.json |]

data Destination = Destination
	{ _destinationEmail :: Text
	, _destinationName :: Text
	}

data MergeVariable = MergeVariable
	{ _mergevariableName :: Text
	, _mergevariableContent :: Text
	}

data MergeOverride = MergeOverride
	{ _mergeoverrideRcpt :: Text
	, _mergeoverrideVars :: [MergeVariable]
	}

data MetadataOverride = MetadataOverride
	{ _metadataoverrideRcpt :: Text
	, _metadataoverrideValues :: [Object]
	}

-- NEED CUSTOM JSON INSTANCE
data Attachment = Attachment
	{ _attachmentMimeType :: Text
	, _attachmentName :: Text
	, _attachmentContent :: ByteString
	}

-- NEED CUSTOM JSON INSTANCE
data Image = Image
	{ _imageMimeType :: Text
	, _imageName :: Text
	, _imageContent :: ByteString
	}

data Message f = Message
	{ _messageHtml :: f Text
	, _messageText :: f Text
	, _messageSubject :: Text
	, _messageFromEmail :: Text
	, _messageFromName :: Text
	, _messageTo :: [Destination]
	, _messageHeaders :: HashMap Text Text
	, _messageImportant :: Bool
	, _messageTrackOpens :: Bool
	, _messageTrackClicks :: Bool
	, _messageAutoText :: Bool
	, _messageAutoHtml :: Bool
	, _messageInlineCss :: Bool
	, _messageUrlStripQs :: Bool
	, _messagePreserveRecipients :: Bool
	, _messageBccAddress :: Text
	, _messageTrackingDomain :: Text
	, _messageSigningDomain :: Text
	, _messageMerge :: Bool
	, _messageGlobalMergeVars :: [MergeVariable]
	, _messageMergeVars :: [MergeOverride]
	, _messageTags :: [Text]
	, _messageGoogleAnalyticsDomains :: [Text]
	, _messageGoogleAnalyticsCampaign :: Text
	, _messageMetadata :: Object
	, _messageRecipientMetadata :: [MetadataOverride]
	, _messageAttachments :: [Attachment]
	, _messageImages :: [Image]
	}

data NewMessage = NewMessage
	{ _newmessageMessage :: Message Identity
	, _newmessageAsync :: Bool
	, _newmessageIpPool :: Maybe Text
	, _newmessageSendAt :: Maybe UTCTime
	}

data Recipient = Recipient
	{ _recipientEmail :: Text
	, _recipientStatus :: EmailStatus
	, _recipientRejectReason :: Text
	, _recipient_id :: Text
	}

-- /messages/send.json
sendMessage :: NewMessage -> Mandrill [Recipient]
sendMessage = post [url| /messages/send.json |]

data NewTemplateMessage = NewTemplateMessage
	{ _newtemplateMessage :: Message Maybe
	, _newtemplateTemplateName :: Text
	, _newtemplateTemplateContent :: [TemplateContent]
	, _newtemplateAsync :: Bool
	, _newtemplateIpPool :: Maybe Text
	, _newtemplateSendAt :: Maybe UTCTime
	}

-- /messages/send-templates.json
sendTemplateMessage :: NewTemplateMessage -> Mandrill [Recipient]
sendTemplateMessage = post [url| /messages/send-templates.json |]

data MessageSearch = MessageSearch
	{ _messagesearchQuery :: Text
	, _messagesearchDateFrom :: Maybe UTCTime
	, _messagesearchDateTo :: Maybe UTCTime
	, _messagesearchTags :: [Text]
	, _messagesearchSenders :: [Text]
	, _messagesearchLimit :: Maybe Int
	}

-- /messages/search.json
searchSentMessages :: MessageSearch -> Mandrill [SentMessage]
searchSentMessages = post [url| /messages/search.json |]

-- /messages/parse.json
parseMessage :: Text -> Mandrill Message
parseMessage = post [url| /messages/parse.json |]

-- /messages/send-raw.json
sendRawMessage :: RawMessage -> Mandrill [Recipient]
sendRawMessage = post [url| /messages/send-raw.json |]

-- /messages/list-scheduled.json
listScheduledMessages :: ScheduledMessageSearchOptions -> Mandrill [ScheduledMessage]
listScheduledMessages = post [url| /messages/list-scheduled.json |]

-- /messages/cancel-scheduled.json
cancelScheduledMessage :: MessageCancellationOptions -> Mandrill CancelledMessage
cancelScheduledMessage = post [url| /messages/cancel-scheduled.json |]

-- /messages/reschedule.json
rescheduleMessage :: RescheduleMessageOptions -> Mandrill RescheduledMessage
rescheduleMessage = post [url| /messages/reschedule.json |]

-- /tags/list.json
listTags :: Mandrill [Tag]
listTags = post [url| /tags/list.json |]

-- /tags/delete.json
deleteTag :: TagName -> Mandrill Tag
deleteTag = post [url| /tags/delete.json |]

-- /tags/info.json
getTagInfo :: TagName -> Mandrill TagInfo
getTagInfo = post [url| /tags/info.json |]

-- /tags/time-series.json
getTagTimeSeries :: TagName -> Mandrill [TagTimeSeriesInfo]
getTagTimeSeries = post [url| /tags/time-series.json |]

-- /tags/all-time-series.json
getAllTagTimeSeries :: Mandrill [TagTimeSeriesInfo]
getAllTagTimeSeries = post [url| /tags/all-time-series.json |]

-- /rejects/add.json
blacklistEmail :: EmailAddress -> Mandrill [BlacklistInfo]
blacklistEmail = post [url| /rejects/add.json |]

-- /rejects/list.json
listBlacklistedEmails :: BlacklistListFilterOptions -> Mandrill [BlacklistedEmail]
listBlacklistedEmails = post [url| /rejects/list.json |]

-- /rejects/delete.json
deleteBlacklistedEmail :: EmailAddress -> Mandrill [BlacklistDeletionInfo]
deleteBlacklistedEmail = post [url| /rejects/delete.json |]

-- /whitelist/add.json
whitelistEmail :: EmailAddress -> Mandrill [WhitelistInfo]
whitelistEmail = post [url| /whitelist/add.json |]

-- /whitelist/list.json
listWhitelistedEmails :: WhitelistListFilterOptions -> Mandrill [WhitelistedEmail]
listWhitelistedEmails = post [url| /whitelist/list.json |]

-- /whitelist/delete.json
deleteWhitelistedEmail :: EmailAddress -> Mandrill [WhitelistDeletionInfo]
deleteWhitelistedEmail = post [url| /whitelist/delete.json |]

-- /senders/list.json
listSenders :: Mandrill [Sender]
listSenders = post [url| /senders/list.json |]

-- /senders/domains.json
listSenderDomains :: Mandrill [Domain]
listSenderDomains = post [url| /senders/domains.json |]

-- /senders/info.json
getSenderInfo :: EmailAddress -> Mandrill [SenderInfo]
getSenderInfo = post [url| /senders/info.json |]

-- /senders/time-series.json
getSenderTimeSeries :: EmailAddress -> Mandrill [SenderTimeSeries]
getSenderTimeSeries = post [url| /senders/time-series.json |]

-- /urls/list.json
listUrls :: Mandrill [UrlInfo]
listUrls = post [url| /urls/list.json |]

-- /urls/search.json
searchUrls :: UrlSearchQuery -> Mandrill [UrlInfo]
searchUrls = post [url| /urls/search.json |]

-- /urls/time-series.json
getUrlTimeSeries :: Url -> Mandrill [UrlInfo]
getUrlTimeSeries = post [url| /urls/time-series.json |]

-- /templates/add.json
addTemplate :: NewTemplate -> Mandrill Template
addTemplate = post [url| /templates/add.json |]

-- /templates/info.json
getTemplateInfo :: TemplateName -> Mandrill Template
getTemplateInfo = post [url| /templates/info.json |]

-- /templates/update.json
updateTemplate :: NewTemplate -> Mandrill Template
updateTemplate = post [url| /templates/update.json |]

-- /templates/publish.json
publishTemplate :: TemplateName -> Mandrill Template
publishTemplate = post [url| /templates/publish.json |]

-- /templates/delete.json
deleteTemplate :: TemplateName -> Mandrill Template
deleteTemplate = post [url| /templates/delete.json |]

-- /templates/list.json
listTemplates :: Mandrill [Template]
listTemplates = post [url| /templates/list.json |]

-- /templates/time-series.json
getTemplateTimeSeries :: TemplateName -> Mandrill Template
getTemplateTimeSeries = post [url| /templates/time-series.json |]

-- /templates/render.json
renderTemplate :: TemplateContent -> Mandrill RenderedTemplate
renderTemplate = post [url| /templates/render.json |]

-- /webhooks/list.json
listWebhooks :: Mandrill [WebHook]
listWebhooks = post [url| /webhooks/list.json |]

-- /webhooks/add.json
addWebhook :: NewWebhook -> Mandrill WebHook
addWebhook = post [url| /webhooks/add.json |]

-- /webhooks/info.json
getWebhookInfo :: WebHookId -> Mandrill WebHook
getWebhookInfo = post [url| /webhooks/info.json |]

-- /webhooks/update.json
updateWebhook :: NewWebhook -> Mandrill WebHook
updateWebhook = post [url| /webhooks/update.json |]

-- /webhooks/delete.json
deleteWebhook :: WebHookId -> Mandrill WebHook
deleteWebhook = post [url| /webhooks/delete.json |]

-- /inbound/domains.json
listInboundDomains :: Mandrill [InboundDomain]
listInboundDomains = post [url| /inbound/domains.json |]

-- /inbound/routes.json
listInboundRoutes :: DomainName -> Mandrill [InboundRoute]
listInboundRoutes = post [url| /inbound/routes.json |]

-- /inbound/send-raw.json
sendRawDocument :: RawMessageInfo -> Mandrill SentRawDocument
sendRawDocument = post [url| /inbound/send-raw.json |]

-- /exports/info.json
getExportInfo :: ExportId -> Mandrill Export
getExportInfo = post [url| /exports/info.json |]

-- /exports/list.json
listExports :: Mandrill [Export]
listExports = post [url| /exports/list.json |]

-- /exports/rejects.json
exportBlacklist :: Email -> Mandrill Export
exportBlacklist = post [url| /exports/rejects.json |]

-- /exports/whitelist.json
exportWhitelist :: Email -> Mandrill Export
exportWhitelist = post [url| /exports/whitelist.json |]

-- /exports/activity.json
exportActivity :: ActivityExportOptions -> Mandrill Export
exportActivity = post [url| /exports/activity.json |]

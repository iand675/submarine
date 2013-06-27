module Network.Mandrill where

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

-- /users/info.json
getUserInfo :: Mandrill User

-- /users/ping.json
ping :: Mandrill Pong

-- /users/ping2.json
ping2 :: Mandrill Pong

-- /users/senders.json
listUserSenders :: Mandrill [Sender]

-- /messages/send.json
sendMessage :: NewMessage -> Mandrill [Recipient]

-- /messages/send-templates.json
sendTemplateMessage :: NewTemplateMessage -> Mandrill [Recipient]

-- /messages/search.json
searchSentMessages :: MessageSearch -> Mandrill [SentMessage]

-- /messages/parse.json
parseMessage :: Text -> Mandrill Message

-- /messages/send-raw.json
sendRawMessage :: RawMessage -> Mandrill [Recipient]

-- /messages/list-scheduled.json
listScheduledMessages :: ScheduledMessageSearchOptions -> Mandrill [ScheduledMessage]

-- /messages/cancel-scheduled.json
cancelScheduledMessage :: MessageCancellationOptions -> Mandrill CancelledMessage

-- /messages/reschedule.json
rescheduleMessage :: RescheduleMessageOptions -> Mandrill RescheduledMessage

-- /tags/list.json
listTags :: Mandrill [Tag]

-- /tags/delete.json
deleteTag :: TagName -> Mandrill Tag

-- /tags/info.json
getTagInfo :: TagName -> Mandrill TagInfo

-- /tags/time-series.json
getTagTimeSeries :: TagName -> Mandrill [TagTimeSeriesInfo]

-- /tags/all-time-series.json
getAllTagTimeSeries :: Mandrill [TagTimeSeriesInfo]

-- /rejects/add.json
blacklistEmail :: EmailAddress -> Mandrill [BlacklistInfo]

-- /rejects/list.json
listBlacklistedEmails :: BlacklistListFilterOptions -> Mandrill [BlacklistedEmail]

-- /rejects/delete.json
deleteBlacklistedEmail :: EmailAddress -> Mandrill [BlacklistDeletionInfo]

-- /whitelist/add.json
whitelistEmail :: EmailAddress -> Mandrill [WhitelistInfo]

-- /whitelist/list.json
listWhitelistedEmails :: WhitelistListFilterOptions -> Mandrill [WhitelistedEmail]

-- /whitelist/delete.json
deleteWhitelistedEmail :: EmailAddress -> Mandrill [WhitelistDeletionInfo]

-- /senders/list.json
listSenders :: Mandrill [Sender]

-- /senders/domains.json
listSenderDomains :: Mandrill [Domain]

-- /senders/info.json
getSenderInfo :: EmailAddress -> Mandrill [SenderInfo]

-- /senders/time-series.json
getSenderTimeSeries :: EmailAddress -> Mandrill [SenderTimeSeries]

-- /urls/list.json
listUrls :: Mandrill [UrlInfo]

-- /urls/search.json
searchUrls :: UrlSearchQuery -> Mandrill [UrlInfo]

-- /urls/time-series.json
getUrlTimeSeries :: Url -> Mandrill [UrlInfo]

-- /templates/add.json
addTemplate :: NewTemplate -> Mandrill Template

-- /templates/info.json
getTemplateInfo :: TemplateName -> Mandrill Template

-- /templates/update.json
updateTemplate :: NewTemplate -> Mandrill Template

-- /templates/publish.json
publishTemplate :: TemplateName -> Mandrill Template

-- /templates/delete.json
deleteTemplate :: TemplateName -> Mandrill Template

-- /templates/list.json
listTemplates :: Mandrill [Template]

-- /templates/time-series.json
getTemplateTimeSeries :: TemplateName -> Mandrill Template

-- /templates/render.json
renderTemplate :: TemplateContent -> Mandrill RenderedTemplate

-- /webhooks/list.json
listWebhooks :: Mandrill [WebHook]

-- /webhooks/add.json
addWebhook :: NewWebhook -> Mandrill WebHook

-- /webhooks/info.json
getWebhookInfo :: WebHookId -> Mandrill WebHook

-- /webhooks/update.json
updateWebhook :: NewWebhook -> Mandrill WebHook

-- /webhooks/delete.json
deleteWebhook :: WebHookId -> Mandrill WebHook

-- /inbound/domains.json
listInboundDomains :: Mandrill [InboundDomain]

-- /inbound/routes.json
listInboundRoutes :: DomainName -> Mandrill [InboundRoute]

-- /inbound/send-raw.json
sendRawDocument :: RawMessageInfo -> Mandrill SentRawDocument

-- /exports/info.json
getExportInfo :: ExportId -> Mandrill Export

-- /exports/list.json
listExports :: Mandrill [Export]

-- /exports/rejects.json
exportBlacklist :: Email -> Mandrill Export

-- /exports/whitelist.json
exportWhitelist :: Email -> Mandrill Export

-- /exports/activity.json
exportActivity :: ActivityExportOptions -> Mandrill Export
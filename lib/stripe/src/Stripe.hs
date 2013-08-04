module Stripe where
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Network.URI.Template

chargesUrl = [uri| /v1/charges |]
chargeUrl chargeId = [uri| /v1/charges/{chargeId} |]
couponsUrl = [uri| /v1/coupons |]
couponUrl couponId = [uri| /v1/coupons/{couponId} |]
customersUrl = [uri| /v1/customers |]
customerUrl customerId = [uri| /v1/customers/{customerId} |]
customerSubscriptionUrl customerId = [uri| /v1/customers/{customerId}/subscription |]
invoicesUrl = [uri| /v1/invoices |]
invoiceUrl invoiceId = [uri| /v1/invoices/{invoiceId} |]
invoiceItemsUrl = [uri| /v1/invoicesitems |]
invoiceItemUrl invoiceItemId = [uri| /v1/invoicesitems/{invoiceItemId} |]
plansUrl = [uri| /v1/plans |]
planUrl planId = [uri| /v1/plans/{planId} |]
eventsUrl = [uri| /v1/events |]
eventUrl eventId = [uri| /v1/events/{eventId} |]
tokensUrl = [uri| /v1/tokens |]
tokenUrl tokenId = [uri| /v1/tokens/{tokenId} |]

data NewBankAccountToken = NewBankAccountToken
	{ _newbankaccountCountry :: Text
	, _newbankaccountRoutingNumber :: Text
	, _newbankaccountAccountNumber :: Text
	}

data NewCharge = NewCharge
	{ _newchargeAmount :: Cents
	, _newchargeCurrency :: Text
	, _newchargeIdentifier :: Either CustomerId CreditCard
	, _newchargeDescription :: Maybe Text
	, _newchargeCapture :: Maybe Bool
	, _newchargeApplicationFee :: Maybe Cents
	}

data NewCardTokenCardInfo = NewCardTokenCardInfo
	{ _newcardtokenNumber :: Text
	, _newcardtokenExpirationMonth :: Int
	, _newcardtokenExpirationYear :: Int
	, _newcardtokenCvc :: Text
	, _newcardtokenName :: Maybe Text
	}

data NewCardToken
	= CardTokenCardInfo NewCardCardInfo
	| CardTokenCustomer NewCardTokenCustomerInfo

data Capture = Capture
	{ _captureChargeId :: Text
	, _captureAmount :: Maybe Cents
	, _captureApplicationFee :: Maybe Bool
	}

data Charge = Charge
	{ _chargeIdentifier :: Text
	, _chargeObject :: Text
	, _chargeCreated :: Integer
	, _chargeLivemode :: Bool
	, _chargePaid :: Bool
	, _chargeAmount :: Cents
	, _chargeCurrency :: Text
	, _chargeRefunded :: Bool
	, _chargeFee :: Maybe Cents
	, _chargeFeeDetails :: Maybe FeeDetails
	, _chargeCard :: CreditCard
	, _chargeCaptured :: Bool
	, _chargeFailureMessage :: Maybe Text
	, _chargeFailureCode :: Maybe Text
	, _chargeAmountRefunded :: Cents
	, _chargeCustomer :: Maybe CustomerId
	, _chargeInvoice :: Maybe invoice
	, _chargeDescription :: Maybe Text
	, _chargeDispute :: Maybe Dispute
	}

data Refund = Refund
	{ _refundChargeId :: Text
	, _refundAmount :: Maybe Cents
	, _refundApplicationFee :: Maybe Bool
	}

newtype Stripe = Stripe { fromStripe :: ReaderT StripeConfig (ResourceT IO) a } deriving (Functor, Applicative, Monad)

-- Stripe API version 2013-02-13


-- create charge
-- POST /v1/charges (NewCharge -> Charge)
createCharge :: NewCharge -> Stripe (Response Charge)
createCharge = post chargesUrl

-- get charge
-- GET /v1/charges/{CHARGE_ID}
getCharge :: ChargeId -> Stripe (Response Charge)
getCharge = get . chargeUrl

-- refund charge
-- POST /v1/charges/{CHARGE_ID}/refund (Refund -> Charge)
refundCharge :: ChargeId -> Refund -> Stripe (Response Charge)
refundCharge = post . (++ "/refund") . chargeUrl

-- capture charge
-- POST /v1/charges/{CHARGE_ID}/capture
captureCharge :: ChargeId -> Capture -> Stripe (Response Charge)
captureCharge = post . (++ "/capture") . chargeUrl

-- list all charges
listCharges :: ListChargesQuery -> Stripe (Response Charges)
listCharges q = get (chargesUrl ++ [uri| {?q*} |])

-- create customer
createCustomer :: NewCustomer -> Stripe (Response Customer)
createCustomer = post customersUrl

-- get customer
getCustomer :: CustomerId -> Stripe (Response Customer)
getCustomer = get . customerUrl

-- update customer
updateCustomer :: CustomerId -> UpdatedCustomer -> Stripe (Response Customer)
updateCustomer cId = post (customerUrl cId)

-- delete customer
deleteCustomer :: CustomerId -> Stripe (Response DeletedResponse)
deleteCustomer = delete . customerUrl

-- list all customers
listCustomers :: ListCustomersQuery -> Stripe (Response Customers)
listCustomers q = get (customersUrl ++ [url| {?q*} |])

-- update subscription
updateCustomerSubscription :: CustomerId -> UpdatedSubscription -> Stripe 
updateCustomerSubscription 

-- cancel subscription
cancelSubscription :: CustomerId -> Stripe DeletedResponse
cancelSubscription = delete . customerId

-- create plan
-- get plan
-- update plan
-- delete plan
-- list all plans

-- create coupon
-- get coupon
-- delete coupon
-- list coupons

-- delete discount

-- get invoice
-- get invoice line items
-- create invoice
-- pay invoice
-- update invoice
-- list invoices
listInvoices :: ListInvoicesQuery -> Stripe Invoices
listInvoices q = get (invoicesUrl ++ [url| {?q*} |])
-- get upcoming invoice

-- create invoice item
createInvoiceItem
-- get invoice item
-- update invoice item
-- delete invoice item
-- list all invoice items

-- update dispute

-- create transfer
-- get transfer
-- cancel transfer
-- list all transfers

-- create recipient
-- get recipient
-- update recipient
-- delete recipient
-- list recipients

-- get account details

-- get balance
-- list balance history

-- get event
-- list all events
-- EVENT WEBHOOKS

-- create card token

-- create bank account token


-- get token

module Network.Stripe where

-- create charge
data NewCharge = NewCharge
	{ _newchargeAmount :: Cents
	, _newchargeCurrency :: Text
	, _newchargeIdentifier :: Either CustomerId CreditCard
	, _newchargeDescription :: Maybe Text
	, _newchargeCapture :: Maybe Bool
	, _newchargeApplicationFee :: Maybe Cents
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

-- get charge
-- refund charge
-- capture charge
-- list all charges

-- create customer
-- get customer
-- update customer
-- delete customer
-- list all customers

-- update subscription
-- cancel subscription

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
-- get upcoming invoice

-- create invoice item
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
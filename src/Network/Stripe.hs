module Network.Stripe where

-- create charge
data NewCharge = NewCharge
	{ _newChargeAmount
	, _newChargeCurrency
	, _newChargeCustomer
	, _newChargeCard
	, _newChargeDescription
	, _newChargeCapture
	, _newChargeApplicationFee
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
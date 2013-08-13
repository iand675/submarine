{-# LANGUAGE QuasiQuotes #-}
module Stripe where
import Data.Aeson
import Data.Aeson.TH
import URI.TH
import Stripe.Types

-- Stripe API version 2013-07-05

-- Charges

-- create charge
createCharge :: NewCharge -> StripeResponse Charge
createCharge = post [uri| /charges{?q*} |]

-- get charge
retrieveCharge :: ChargeId -> StripeResponse Charge
retrieveCharge = get [uri| /charges/{chargeId} |]

-- refund charge
refundCharge :: ChargeId -> Refund -> StripeResponse Charge
refundCharge = post [uri| /charges/{chargeId}/refund |]

-- capture charge
captureCharge :: ChargeId -> Capture -> StripeResponse Charge
captureCharge chargeId = post [uri| /charges/{chargeId}/capture |]

-- list all charges
listCharges :: ListChargesQuery -> StripeResponse (List Charge)
listCharges q = get [uri| /charges{?q*} |]

-- Customers

-- create customer
createCustomer :: NewCustomer -> StripeResponse Customer
createCustomer = post [uri| /customers |]

-- get customer
retrieveCustomer :: CustomerId -> StripeResponse Customer
retrieveCustomer customerId = get [uri| /customers/{customerId} |]

-- update customer
updateCustomer :: CustomerId -> UpdatedCustomer -> StripeResponse Customer
updateCustomer customerId = post [uri| /customers/{customerId} |]

-- delete customer
deleteCustomer :: CustomerId -> StripeResponse DeletedResponse
deleteCustomer customerId = delete [uri| /customers/{customerId} |]

-- list all customers
listCustomers :: ListCustomersQuery -> StripeResponse (List Customer)
listCustomers q = get [uri| /customers{?q} |]

-- Cards

-- create a new card
createCard :: CustomerId -> NewCard -> StripeResponse Card
createCard customerId = post [uri| /customers/{customerId}/cards |]

-- retrieve an existing card
retrieveExistingCard :: CustomerId -> CardId -> StripeResponse Card
retrieveExistingCard customerId cardId = get [uri| /customers/{customerId}/cards/{cardId} |]

-- update a card
updateCard :: CustomerId -> CardId -> UpdatedCard -> StripeResponse Card
updateCard customerId cardId = post [uri| /customers/{customerId}/cards/{cardId} |]

-- delete a card
deleteCard :: CustomerId -> CardId -> StripeResponse Card
deleteCard customerId cardId = delete [uri| /customers/{customerId}/cards/{cardId} |]

-- list all cards
listCards :: CustomerId -> Count -> Offset -> StripeResponse (List Card)
listCards customerId count offset = get [uri| /customers/{customerId}/cards{?count, offset} |]

-- Subscriptions

updateSubscription :: CustomerId -> UpdatedSubscription -> StripeResponse Subscription
updateSubscription customerId = post [uri| /customers/{customerId}/subscription |]

data CancelTime = AtPeriodEnd | Now
cancelSubscription :: CustomerId -> CancelTime -> StripeResponse Subscription
cancelSubscription customerId = delete [uri| /customers/{customerId}/subscription |]

-- Plans

createPlan :: NewPlan -> StripeResponse Plan
createPlan = post [uri| /plans |]

retrievePlan :: PlanId -> StripeResponse Plan
retrievePlan planId = get [uri| /plans/{planId} |]

updatePlan :: PlanId -> UpdatedPlan -> StripeResponse Plan
updatePlan planId = post [uri| /plans/{planId} |]

deletePlan :: PlanId -> StripeResponse DeletedPlan
deletePlan planId = delete [uri| /plans/{planId} |]

listPlans :: Count -> Offset -> StripeResponse (List Plan)
listPlans count offset = get [uri| /plans{?count,offset} |]

-- Coupons

createCoupon :: NewCoupon -> StripeResponse Coupon
createCoupon = post [uri| /coupons |]

retrieveCoupon :: CouponId -> StripeResponse Coupon
retrieveCoupon couponId = get [uri| /coupons/{couponId} |]

deleteCoupon :: CouponId -> StripeResponse DeletedCoupon
deleteCoupon couponId = delete [uri| /coupons/{couponId} |]

listCoupons :: Count -> Offset -> StripeResponse (List Coupon)
listCoupons = get [uri| /coupons{?count,offset} |]

-- Discounts

deleteDiscount :: CustomerId -> StripeResponse DeletedDiscount
deleteDiscount customerId = delete [uri| /customers/{customerId}/discount |]

-- Invoices

retrieveInvoice :: InvoiceId -> StripeResponse Invoice
retrieveInvoice invoiceId = get [uri| /invoices/{invoiceId} |]

retrieveLineItems :: InvoiceId -> Count -> Offset -> CustomerId -> Stripe (List LineItem)
retrieveLineItems invoiceId count offset customerId = get [uri| /invoices/{invoiceId}/lines{?count,offset,customerId} |]

-- TODO: need to make json object for customer id here
createInvoice :: CustomerId -> StripeResponse Invoice
createInvoice = post [uri| /invoices |]

payInvoice :: InvoiceId -> StripeResponse Invoice
payInvoice invoiceId = post [uri| /invoices/{invoiceId}/pay |]

updateInvoice :: InvoiceId -> UpdatedInvoice -> StripeResponse Invoice
updateInvoice invoiceId = post [uri| /invoices/{invoiceId} |]

listInvoices :: Count -> Offset -> CustomerId -> Date -> StripeResponse (List Invoice)
listInvoices count offset customer date = get [uri| /invoices{?count,offset,customer,date} |]

getUpcomingInvoice :: CustomerId -> StripeResponse Invoice
getUpcomingInvoice customer = get [uri| /invoices/upcoming{?customer} |]

-- Invoice items

createInvoiceItem :: NewInvoiceItem -> StripeResponse InvoiceItem
createInvoiceItem = post [uri| /invoiceitems |]

retrieveInvoiceItem :: InvoiceItemId -> StripeResponse InvoiceItem
retrieveInvoiceItem invoiceItemId = get [uri| /invoiceitems/{invoiceItemId} |]

updateInvoiceItem :: InvoiceItemId -> UpdatedInvoice -> StripeResponse InvoiceItem
updateInvoiceItem invoiceItemId = post [uri| /invoiceitems/{invoiceItemId} |]

deleteInvoiceItem :: InvoiceItemId -> StripeResponse DeletedInvoiceItem
deleteInvoiceItem invoiceItemId = post [uri| /invoiceitems/{invoiceItemId} |]

listInvoiceItems :: Count -> Offset -> CustomerId -> Date -> StripeResponse (List InvoiceItem)
listInvoiceItems count offset customer date = get [uri| /invoices{?count,offset,customer,date} |]

-- Disputes

updateDispute :: ChargeId -> Evidence -> StripeResponse Dispute
updateDispute chargeId evidence = post [uri| /charges/{chargeId}/dispute |]

-- Transfers

createTransfer :: NewTransfer -> StripeResponse Transfer
createTransfer = post [uri| /transfers |]

retrieveTransfer :: TransferId -> StripeResponse Transfer
retrieveTransfer transferId = get [uri| /transfers/{transferId} |]

cancelTransfer :: TransferId -> StripeResponse Transfer
cancelTransfer transferId = get [uri| /transfers/{transferId}/cancel |]

listTransfers :: Count -> Offset -> Date -> RecipientId -> TransferStatus -> StripeResponse (List Transfer)
listTransfers count offset date recipient status = get [uri| /transfers{?count,offset,date,recipient,status} |]

-- Recipients

createRecipient :: NewRecipient -> StripeResponse Recipient
createRecipient = post [uri| /recipients |]

retrieveRecipient :: RecipientId -> StripeResponse Recipient
retrieveRecipient recipientId = get [uri| /recipients/{recipientId} |]

updateRecipient :: RecipientId -> UpdatedRecipient -> StripeResponse Recipient
updateRecipient recipientId = post [uri| /recipients/{recipientId} |]

deleteRecipient :: RecipientId -> StripeResponse DeletedRecipient
deleteRecipient recipientId = delete [uri| /recipients/{recipientId} |]

listRecipients :: Count -> Offset -> VerificationStatus -> StripeResponse (List Recipient)
listRecipients count offset verified = get [uri| /recipients{?count, offset, verified} |]

-- Account

retrieveAccount :: StripeResponse Account
retrieveAccount = get [uri| /account |]

-- Balance

retrieveBalance :: StripeResponse Balance
retrieveBalance = get [uri| /balance |]

listBalanceHistory :: Count -> Offset -> AvailableOn -> Created -> TransferId -> TransactionType -> StripeResponse (List Transaction)
listBalanceHistory count offset available_on created transfer = get [uri| /balance/history{?count,offset,available_on,created,transfer,type}|]

-- Events

retrieveEvent :: EventId -> StripeResponse Event
retrieveEvent eventId = get [uri| /events/{eventId} |]

listEvents :: Count -> Offset -> Created -> EventType -> StripeResponse (List Event)
listEvents count offset created = get [uri| /events{?count,offset,created,type} |]

-- Tokens

-- This encapsulates both card & bank account tokens
createToken :: NewToken -> StripeResponse Token
createToken = post [uri| /tokens |]

retrieveToken :: TokenId -> StripeResponse Token
retrieveToken tokenId = get [uri| /tokens/{tokenId} |]



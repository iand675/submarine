module EventHandler where

"account.updated" :: Account
"account.application.deauthorized" :: Application
"balance.available" :: Balance
"charge.succeeded" :: Charge
"charge.failed" :: Charge
"charge.refunded" :: Charge
"charge.captured" :: Charge
"charge.dispute.created" :: Dispute
"charge.dispute.updated" :: Dispute
"charge.dispute.closed" :: Dispute
"customer.created" :: Customer
"customer.updated" :: Customer
"customer.deleted" :: Customer
"customer.card.created" :: Card
"customer.card.updated" :: Card
"customer.card.deleted" :: Card
"customer.subscription.created" :: Subscription
"customer.subscription.deleted" :: Subscription
"customer.subscription.trial_will_end" :: Subscription
"customer.discount.created" :: Discount
"customer.discount.updated" :: Discount
"customer.discount.deleted" :: Discount
"invoice.created" :: Invoice
"invoice.updated" :: Invoice
"invoice.payment_succeeded" :: Invoice
"invoice.payment_failed" :: Invoice
"invoiceitem.created" :: InvoiceItem
"invoiceitem.updated" :: InvoiceItem
"invoiceitem.deleted" :: InvoiceItem
"plan.created" :: Plan
"plan.updated" :: Plan
"plan.deleted" :: Plan
"coupon.created" :: Coupon
"coupon.deleted" :: Coupon
"transfer.created" :: Transfer
"transfer.updated" :: Transfer
"transfer.paid" :: Transfer
"transfer.failed" :: Transfer
"ping" :: Maybe Object

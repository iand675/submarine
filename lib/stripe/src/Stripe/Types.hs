module Stripe.Types where
import Control.Applicative
import Control.Monad.Reader
import Data.Text (Text)

type CustomerId = Text
type Cents = Int

newtype Stripe = Stripe { fromStripe :: ReaderT StripeConfig (ResourceT IO) a }
  deriving (Functor, Applicative, Monad)

data Charge = Charge
  { chargeId :: ChargeId
  , chargeObject :: Text
  , chargeLivemode :: Bool
  , chargeAmount :: Int
  , chargeCaptured :: Bool
  , chargeCard :: Card
  , chargeCreated :: Int
  , chargeCurrency :: Text
  , chargeFee :: Int
  , chargeFeeDetails :: FeeDetails
  , chargePaid :: Bool
  , chargeRefunded :: Bool
  , chargeAmountRefunded :: Int
  , chargeCustomer :: Maybe CustomerId
  , chargeDescription :: Text
  , chargeDispute :: Maybe Dispute
  , chargeFailureCode :: Maybe Text
  , chargeFailureMessage :: Maybe Text
  , chargeInvoice :: Maybe InvoiceId
  }

data Customer = Customer
  { customerId :: CustomerId
  , customerObject :: Text
  , customerLivemode :: Bool
  , customerCards :: List Card
  , customerCreated :: Int
  , customerAccountBalance :: Int
  , customerDefaultCard :: Maybe CardId
  , customerDelinquent :: Bool
  , customerDescription :: Maybe Text
  , customerDiscount :: Maybe Discount
  , customerEmail :: Text
  , customerSubscription :: Maybe Subscription
  }

data CardInfoCheckStatus = Pass | Fail | Unchecked

data Card = Card
  { cardId :: CardId
  , cardObject :: Text
  , cardExpMonth :: Int
  , cardExpYear :: Int
  , cardFingerprint :: Text
  , cardLast4 :: Text
  , cardType :: Text
  , cardAddressCity :: Maybe Text
  , cardAddressCountry :: Maybe Text
  , cardAddressLine1 :: Maybe Text
  , cardAddressLine1Check :: Maybe CardInfoCheckStatus
  , cardAddressLine2 :: Maybe Text
  , cardAddressState :: Maybe Text
  , cardAddressZip :: Maybe Text
  , cardAddressZipCheck :: Maybe CardInfoCheckStatus
  , cardCountry :: Maybe Text
  , cardCustomer :: CustomerId
  , cardCvcCheck :: Maybe CardInfoCheckStatus
  , cardName :: Maybe Text
  }

data Subscription = Subscription
  { subscriptionObject :: Text
  , subscriptionCancelAtPeriodEnd :: Bool
  , subscriptionCustomer :: CustomerId
  , subscriptionPlan :: Plan
  , subscriptionQuantity :: Int
  , subscriptionStart :: Int
  , subscriptionStatus :: SubscriptionStatus
  , subscriptionCanceledAt :: Maybe Int
  , subscriptionCurrentPeriodEnd :: Int
  , subscriptionCurrentPeriodStart :: Int
  , subscriptionEndedAt :: Maybe Int
  , subscriptionTrialEnd :: Maybe Int
  , subscriptionTrialStart :: Maybe Int
  }

data PlanInterval = Week | Month | Year

data Plan = Plan
  { planId :: PlanId
  , planObject :: Text
  , planLivemode :: Bool
  , planAmount :: Int
  , planCurrency :: Text
  , planInterval :: PlanInterval
  , planIntervalCount :: Int
  , planName :: Text
  , planTrialPeriodDays :: Maybe Int
  }

data CouponDuration = Forever | Once | Repeating

data Coupon = Coupon
  { couponId :: CouponId
  , couponObject :: Text
  , couponLivemode :: Bool
  , couponDuration :: CouponDuration
  , couponAmountOff :: Int
  , couponCurrency :: Text
  , couponDurationInMonths :: Maybe Int
  , couponMaxRedemptions :: Maybe Int
  , couponRedeemBy :: Maybe Int
  , couponTimesRedeemed :: Int
  }

data Discount = Discount
  { discountObject :: Text
  , discountCoupon :: Coupon
  , discountCustomer :: CustomerId
  , discountStart :: Int
  , discountEnd :: Maybe Int
  }

data Invoice = Invoice
  { invoiceId :: InvoiceId
  , invoiceObject :: Text
  , invoiceLivemode :: Bool
  , invoiceAmountDue :: Int
  , invoiceAttemptCount :: Int
  , invoiceAttempted :: Bool
  , invoiceClosed :: Bool
  , invoiceCurrency :: Text
  , invoiceCustomer :: CustomerId
  , invoiceDate :: Int
  , invoiceLines :: List InvoiceItem
  , invoicePaid :: Bool
  , invoicePeriodEnd :: Int
  , invoiceStartingBalance :: Int
  , invoiceSubtotal :: Int
  , invoiceTotal :: Int
  , invoiceCharge :: Maybe ChargeId
  , invoiceDiscount :: Maybe Discount
  , invoiceEndingBalance :: Maybe Int
  , invoiceNextPaymentAttempt :: Maybe Int
  }

data InvoiceItem = InvoiceItem
  { invoiceItemId :: InvoiceItemId
  , invoiceItemObject :: Text
  , invoiceItemLivemode :: Bool
  , invoiceItemAmount :: Int
  , invoiceItemCurrency :: Text
  , invoiceItemCustomer :: CustomerId
  , invoiceItemDate :: Int
  , invoiceItemProration :: Bool
  , invoiceItemDescription :: Maybe String
  , invoiceItemInvoice :: Maybe InvoiceId
  }

data DisputeReason = Duplicate | Fraudulent | SubscriptionCanceled | ProductUnacceptable | ProductNotReceived | Unrecognized | CreditNotProcessed | General
data DisputeStatus = Won | Lost | NeedsResponse | UnderReview

data Dispute = Dispute
  { disputeObject :: Text
  , disputeLivemode :: Bool
  , disputeAmount :: Int
  , disputeCharge :: ChargeId
  , disputeCreated :: Int
  , disputeCurrency :: Text
  , disputeReason :: DisputeReason
  , disputeStatus :: DisputeStatus
  , disputeEvidence :: Maybe Text
  , disputeEvidenceDueBy :: Int
  }

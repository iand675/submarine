{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stripe.Types where
import Control.Applicative
import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Conduit (Response)

type TokenId = Text
type CustomerId = Text
type CouponId = Text
type PlanId = Text
type InvoiceId = Text
type ChargeId = Text
type TransferId = Text
type RecipientId = Text
type InvoiceItemId = Text
type RequestId = Text
type AccountId = Text
type EventId = Text
type CardId = Text
type Offset = Int
type Count = Int
type Created = Int
type AvailableOn = Int
type Date = Int
type Evidence = Text
type DeletedInvoiceItem = DeletedResponse
type DeletedCoupon = DeletedResponse
type DeletedDiscount = DeletedResponse
type DeletedRecipient = DeletedResponse
type DeletedPlan = DeletedResponse

type StripeResponse a = Stripe (Response a)

data StripeConfig = StripeConfig { apiKey :: Text }

newtype Stripe a = Stripe { fromStripe :: ReaderT StripeConfig IO a }
  deriving (Functor, Applicative, Monad)

data List a = List
  { listObject :: Text
  , listUrl :: Text
  , listCount :: Int
  , listData :: [a]
  }

data VerificationStatus = Verified | Unverified

data DeletedResponse = DeletedResponse
  { deletedResponseId :: Text
  , deletedResponseDeleted :: Bool
  }

data NewCharge = NewCharge
  { newChargeAmount :: Int
  , newChargeCurrency :: Text
  , newChargeCustomer :: Maybe CustomerId
  , newChargeCard :: Maybe (Either TokenId NewCard)
  , newChargeDescription :: Maybe Text
  , newChargeCapture :: Maybe Bool
  , newChargeApplicationFee :: Maybe Int
  }

data Refund = Refund
  { refundId :: ChargeId
  , refundAmount :: Maybe Int
  , refundApplicationFee :: Maybe Int
  }

data Capture = Capture
  { captureId :: ChargeId
  , captureAmount :: Maybe Int
  , captureApplicationFee :: Maybe Int
  }

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

data NewCustomer = NewCustomer
  { newCustomerAccountBalance :: Maybe Int
  , newCustomerCard :: Either TokenId NewCardInfo
  , newCustomerCoupon :: Maybe Text
  , newCustomerDescription :: Maybe Text
  , newCustomerEmail :: Maybe Text
  , newCustomerPlan :: Maybe PlanId
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

data NewCardInfo = NewCardInfo
  { newCardInfoNumber :: Text
  , newCardInfoExpMonth :: Int
  , newCardInfoExpYear :: Int
  , newCardInfoCvc :: (Maybe Int)
  , newCardInfoName :: (Maybe Text)
  , newCardInfoAddressLine1 :: Maybe Text
  , newCardInfoAddressLine2 :: Maybe Text
  , newCardInfoAddressZip :: Maybe Text
  , newCardInfoAddressState :: Maybe Text
  , newCardInfoAddressCountry :: Maybe Text
  }

data NewCard = NewCard
  { newCardCard :: Either TokenId NewCardInfo
  }

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

data SubscriptionStatus = Trialing | Active | PastDue | Canceled | Unpaid

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

data NewPlan = NewPlan
  { newPlan :: PlanId
  , newPlanAmount :: Int
  , newPlanCurrency :: Text
  , newPlanInterval :: PlanInterval
  , newPlanIntervalCount :: Maybe Int
  , newPlanName :: Text
  , newPlanTrialPeriodDays :: Maybe Int
  }

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

data NewCoupon = NewCoupon
  { newCouponId :: Maybe Text
  , newCouponDuration :: CouponDuration
  , newCouponAmountOff :: Maybe Int
  , newCouponCurrency :: Maybe Text
  , newCouponDurationInMonths :: Maybe Int
  , newCouponMaxRedemptions :: Maybe Int
  , newCouponPercentOff :: Maybe Int
  , newCouponRedeemBy :: Int
  }

data Coupon = Coupon
  { couponId :: CouponId
  , couponObject :: Text
  , couponLivemode :: Bool
  , couponDuration :: CouponDuration
  , couponAmountOff :: Maybe Int
  , couponCurrency :: Text
  , couponDurationInMonths :: Maybe Int
  , couponMaxRedemptions :: Maybe Int
  , couponPercentOff :: Maybe Int
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

data NewInvoiceItem = NewInvoiceItem
  { newInvoiceItemCustomer :: CustomerId
  , newInvoiceItemAmount :: Int
  , newInvoiceItemCurrency :: Text
  , newInvoiceItemInvoice :: Maybe InvoiceId
  , newInvoiceItemDescription :: Maybe Text
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

data FeeDetails = FeeDetails
  { feeDetailsAmount :: Int
  , feeDetailsCurrency :: Text
  , feeDetailsType :: Text
  , feeDetailsApplication :: Maybe Text
  , feeDetailsDescription :: Maybe Text
  }

data TransferStatus = Paid | TransferPending | Failed

data BankAccount = BankAccount
  { bankAccountObject :: Text
  , bankAccountBankName :: Text
  , bankAccountCountry :: Text
  , bankAccountLast4 :: Text
  , bankAccountFingerprint :: Maybe Text
  , bankAccountValidated :: Maybe Bool
  }

data NewTransfer = NewTransfer
  { newTransferAmount :: Int
  , newTransferCurrency :: Text
  , newTransferRecipient :: RecipientId
  , newTransferDescription :: Maybe Text
  , newTransferStatementDescription :: Maybe Text
  }

data Transfer = Transfer
  { transferId :: TransferId
  , transferObject :: Text
  , transferLivemode :: Bool
  , transferAmount :: Int
  , transferCurrency :: Text
  , transferDate :: Int
  , transferFee :: Int
  , transferFeeDetails :: [FeeDetails]
  , transferStatus :: TransferStatus
  , transferAccount :: Maybe BankAccount
  , transferDescription :: Maybe Text
  , transferRecipient :: Maybe RecipientId
  , transferStatementDescriptor :: Maybe Text
  }

data RecipientType = Individual | Corporation

data NewBankAccount = NewBankAccount
  { newBankAccountCountry :: Text
  , newBankAccountRoutingNumber :: Text
  , newBankAccountAccountNumber :: Text
  , newBankAccountEmail :: Maybe Text
  , newBankAccountOptional :: Maybe Text
  }

data NewRecipient = NewRecipient
  { newRecipientName :: Text
  , newRecipientType :: RecipientType
  , newRecipientTaxId :: Maybe Text
  , newRecipientBankAccount :: Maybe NewBankAccount
  }

data Recipient = Recipient
  { recipientId :: RecipientId
  , recipientObject :: Text
  , recipientLivemode :: Bool
  , recipientCreated :: Int
  , recipientType :: RecipientType
  , recipientActiveAccount :: Maybe BankAccount
  , recipientDescription :: Maybe Text
  , recipientEmail :: Maybe Text
  , recipientName :: Text
  }

data Account = Account
  { accountId :: AccountId
  , accountObject :: Text
  , accountChargeEnabled :: Bool
  , accountCountry :: Text
  , accountCurrenciesSupported :: [Text]
  , accountDefaultCurrency :: Text
  , accountDetailsSubmitted :: Bool
  , accountTransferEnabled :: Bool
  , accountEmail :: Text
  , accountStatementDescriptor :: Maybe Text
  }

data FundInfo = FundInfo
  { fundInfoAmount :: Int
  , fundInfoCurrency :: Text
  }

data Balance = Balance
  { balanceObject :: Text
  , balanceLivemode :: Bool
  , balanceAvailable :: [FundInfo]
  , balancePending :: [FundInfo]
  }

data TransactionStatus = Available | TransactionPending
data TransactionType = ChargeTransaction | RefundTransaction | Adjustment | ApplicationFee | ApplicationFeeRefund | TransferTransaction | TransferCancel | TransferFailure

data BalanceTransaction = BalanceTransaction
  { balanceTransactionObject :: Text
  , balanceTransactionAmount :: Int
  , balanceTransactionAvailableOn :: Int
  , balanceTransactionCreated :: Int
  , balanceTransactionCurrency :: Text
  , balanceTransactionNet :: Int
  , balanceTransactionStatus :: TransactionStatus
  , balanceTransactionType :: TransactionType
  , balanceTransactionDescription :: Maybe Text
  , balanceTransactionFee :: Int
  }

data Event = Event
  { eventId :: EventId
  , eventObject :: Text
  , eventLivemode :: Bool
  , eventCreated :: Int
  , eventData :: Object
  , eventPendingWebhooks :: Int
  , eventType :: Text
  , eventRequest :: RequestId
  }

data TokenType = CardToken | BankAccountToken

data NewToken = NewToken
  { newTokenCard :: Maybe NewCardInfo
  , newTokenCustomer :: Maybe CustomerId
  }

data Token = Token
  { tokenId :: TokenId
  , tokenObject :: Text
  , tokenLivemode :: Bool
  , tokenCreated :: Int
  , tokenType :: TokenType
  , tokenUsed :: Bool
  , tokenBankAccount :: Maybe BankAccount
  , tokenCard :: Maybe Card
  }


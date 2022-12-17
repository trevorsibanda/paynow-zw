{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Payments.Paynow
  ( 
    newPaynowClient,
    defaultProdConfig,
    PaynowConfig (..),
    PaynowClient (..),
    PaynowTransaction (..),
    Checkout (..),
    PaynowTXResult (..),
    PaymentMethod (..),
    TxData (..),
    PaynowError(..),
    PaynowTXPayload (..),
    Status(..),
    BillingAddress (..),
    Card (..),
    PhoneNumber,
    Email,
    Amount,
    Ref,
    PaynowRef,
    PaynowIntegrationID,
    PaynowIntegrationKey,
    PaynowResultURL,
    PaynowReturnURL,
    Hash,
    RedirectURL,
    PollURL,
    VMC (..),
    serverResponseToResult,
    sha1Hex,
  )
where

import UnliftIO.Exception
import UnliftIO
import qualified Crypto.Hash as C
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text hiding (zip, filter, foldr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Base (urlDecode, urlEncodeVars)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Simple

prodPaynowEndpoint :: Text
prodPaynowEndpoint = "https://www.paynow.co.zw/interface/"


-- | Additional data to be sent with the transaction
data TxData = TxData
  { tAdditionalInfo :: Maybe Text,
    tResultURL :: Maybe PaynowResultURL,
    tReturnURL :: Maybe PaynowReturnURL
  }

instance ToKeyMap (Maybe TxData) where
  toKeyMap Nothing = []
  toKeyMap (Just (TxData {..})) = foldr 
    (\(k, v) accum -> if (isJust v) then (k, fromMaybe "" v) : accum else accum )
    []
    [("additionalinfo", tAdditionalInfo), ("returnurl", tReturnURL), ("resulturl", tResultURL)]

class ToKeyMap a where
  toKeyMap :: a -> [(Text, Text)]


-- Type aliases used to make documentation more readable
type PhoneNumber = Text
type Email = Text
type Amount = Double
type PaynowRef = Text
type Instructions = Text

data Status = Created | Error | Cancelled | Paid | Okay | Refunded | Failed | OtherStatus Text deriving (Show, Eq)
type PollURL = Text
type RedirectURL = Text
type Hash = Text
type Ref = Text

type PaynowIntegrationID = Text
type PaynowIntegrationKey = Text
type PaynowResultURL = Text
type PaynowReturnURL = Text


-- | Card Details
data Card = Card {cardNumber :: !Text, cardCVV :: !Text, cardExpiry :: !Text, cardName :: !Text} deriving (Show)

-- | ToKeyMap typeclass
instance ToKeyMap Card where
  toKeyMap Card {..} = [("cardnumber", cardNumber), ("cardname", cardName), ("cardcvv", cardCVV), ("cardexpiry", cardExpiry)]

-- | Billing Address
data BillingAddress = BillingAddress {baAddressLine1 :: !Text, baAddressLine2 :: !Text, baCity :: !Text, baCountry :: !Text, baProvince :: Maybe Text} deriving (Show)

instance ToKeyMap BillingAddress where
  toKeyMap BillingAddress {..} =
    [("billingline1", baAddressLine1), ("billingline2", baAddressLine2), ("billingcity", baCity), ("billingprovince", fromMaybe "" baProvince), ("billingcountry", baCountry)]

-- | Visa Mastercard Details
data VMC = VMC Card BillingAddress deriving (Show)

instance ToKeyMap VMC where
  toKeyMap (VMC card address) = (toKeyMap card) <> (toKeyMap address)

-- | Ecocash, OneMoney, VisaMastercard payment methods
data PaymentMethod = Ecocash PhoneNumber | OneMoney PhoneNumber | VisaMastercard VMC deriving (Show)

instance ToKeyMap PaymentMethod where
  toKeyMap (Ecocash phone) = [("phone", phone), ("method", "ecocash")]
  toKeyMap (OneMoney phone) = [("phone", phone), ("method", "onemoney")]
  toKeyMap (VisaMastercard vmcd) = toKeyMap vmcd

-- | Checkout type
data Checkout = ExpressCheckout PaymentMethod | ClassicCheckout deriving (Show)

instance ToKeyMap Checkout where
  toKeyMap ClassicCheckout = []
  toKeyMap (ExpressCheckout method) = toKeyMap method

data PaynowError = ServerError | InternalError | UnHandledException SomeException | OtherError Text deriving (Show)

-- |
-- | PaynowTXResult is the result of a transaction
-- | 
data PaynowTXResult = 
  InitExpressPaymentSuccess Status PaynowRef Instructions PollURL  Hash | 
  InitClassicCheckoutSuccess Status RedirectURL PollURL Hash  |
  PollPaymentSuccess Status PaynowRef Ref Amount PollURL Hash  |
  VerifySignatureSuccess deriving (Show, Eq)

-- |
--  PaynowTransaction is the type of transaction to be performed.
--
--  InitPayment: Initialize a payment
--  PollPayment: Poll for the status of a payment
--  VerifyStatusUpdate: Verify the authenticity of a status update
data PaynowTransaction = InitPayment Ref Amount Email Checkout (Maybe TxData) | PollPayment PollURL | VerifyStatusUpdate [(Text, Text)]

statusMessage :: [(Text, Text)]
statusMessage = [("status", "Message")]

-- | Ordering of keys for hashing and form body
txOrdering :: [Text]
txOrdering = ["resulturl", "returnurl", "reference", "amount", "id", "additionalinfo", "authemail", "phone", "method", "status", "hash"]

orderTXKeyMap :: [(Text, Text)] -> [(Text, Text)]
orderTXKeyMap l =
  catMaybes $ (\k -> (k,) <$> M.lookup k m) <$> txOrdering
  where
    m :: M.Map Text Text
    m = M.fromList l

instance ToKeyMap PaynowTransaction where
  toKeyMap (InitPayment ref amount email checkout mTxData) =
    orderTXKeyMap $ statusMessage <> [("reference", ref), ("amount", T.pack $ show amount), ("authemail", email)] <> (toKeyMap checkout) <> (toKeyMap mTxData)
  toKeyMap (PollPayment _) = statusMessage
  toKeyMap _ = statusMessage

class PaynowTXPayload a where
  urlParamsWithHash :: a -> PaynowConfig -> (Hash, Text)
  hash :: a -> PaynowConfig -> Text

urlEncodeT :: [(Text, Text)] -> Text
urlEncodeT tple = T.pack $ urlEncodeVars $ (\(k, v) -> (T.unpack k, T.unpack v)) <$> tple

sha1Hex :: Text -> Hash
sha1Hex t =
  T.toUpper $ T.decodeUtf8 $ (C.digestToHexByteString (C.hash (T.encodeUtf8 t) :: C.Digest C.SHA512))

instance PaynowTXPayload PaynowTransaction where
  hash tx config =
    sha1Hex $ (intercalate "" ((snd) <$> (orderTXKeyMap $ toKeyMap config <> toKeyMap tx))) <> (pncIntegrationKey config)

  urlParamsWithHash tx config =
    (hashed, urlEncodeT $ orderTXKeyMap ((toKeyMap config <> toKeyMap tx <> [("hash", hashed)])))
    where
      hashed :: Text
      hashed = hash tx config

-- |
--  PaynowConfig is the configuration for the Paynow API.
--  For more details, see https://www.paynow.co.zw/docs/api/
--
--  pncEndpoint: The endpoint to use for the API
--  pncIntegrationID: The integration ID for the API
--  pncIntegrationKey: The integration key for the API
--  pncResultURL: The URL to redirect to after a transaction is completed
--  pncReturnURL: The URL to redirect to after a transaction is completed
data PaynowConfig = PaynowConfig
  { pncEndpoint :: !Text,
    pncIntegrationID :: !PaynowIntegrationID,
    pncIntegrationKey :: !PaynowIntegrationKey,
    pncResultURL :: !PaynowResultURL,
    pncReturnURL :: !PaynowReturnURL
  }
  deriving (Show)

checkoutEndpoint :: PaynowConfig -> Checkout -> Text
checkoutEndpoint PaynowConfig {..} ClassicCheckout =
  pncEndpoint <> "initiatetransaction"
checkoutEndpoint PaynowConfig {..} (ExpressCheckout _) =
  pncEndpoint <> "remotetransaction"

instance ToKeyMap PaynowConfig where
  toKeyMap PaynowConfig {..} =
    [("id", pncIntegrationID), ("resulturl", pncResultURL), ("returnurl", pncReturnURL)]

-- |
-- | A PaynowClient is a client for the Paynow API. It contains the following functions:
-- | - newExpressCheckout: Create a new express checkout transaction (mobile or visa mastercard)
-- | - newClassicCheckout: Create a new classic checkout transaction (user will be redirected to url to complete payment)
-- | - newPollPayment: Create a new poll payment transaction (poll for the status of a payment)
-- | - newVerifyStatusUpdate: Create a new verify status update transaction (verify the authenticity of a status update)
-- | - processTx: Process a transaction, sends an HTTP request to the Paynow API
-- | - txHash: Get the hash of a transaction
data PaynowClient m = PaynowClient
  { newExpressCheckout :: PaymentMethod -> Ref -> Amount -> Email -> Maybe TxData -> m PaynowTransaction,
    newClassicCheckout :: Ref -> Amount -> Email -> Maybe TxData -> m PaynowTransaction,
    newPollPayment :: PollURL -> m PaynowTransaction,
    newVerifyStatusUpdate :: [(Text, Text)] -> m PaynowTransaction,
    processTx :: PaynowTransaction -> m (Either PaynowError PaynowTXResult),
    txHash :: PaynowTransaction -> m Text
  }

-- | The default configuration for the Paynow production API
defaultProdConfig :: PaynowIntegrationID -> PaynowIntegrationKey -> PaynowResultURL -> PaynowReturnURL -> PaynowConfig
defaultProdConfig apiID apiSecret = PaynowConfig prodPaynowEndpoint apiID apiSecret



-- |
--  Create a new PaynowClient with the given configuration.
-- @
-- client <- newPaynowClient config
-- payment <- newExpressCheckout (Ecocash "0779800700") "45" 37.50 Nothing
-- result <- processTx payment
-- case result of
--    PaynowError e => //handle err
--    PaynowTxResult r => //handle result
-- @
newPaynowClient :: forall m. (MonadUnliftIO m) => PaynowConfig -> (PaynowClient m)
newPaynowClient config =
  PaynowClient {..}
  where
    newExpressCheckout method ref amount email mTXData =
      pure $ InitPayment ref amount email (ExpressCheckout method) mTXData

    newClassicCheckout ref amount email mTXData =
      pure $ InitPayment ref amount email ClassicCheckout mTXData

    newPollPayment = pure . PollPayment

    newVerifyStatusUpdate = pure . VerifyStatusUpdate

    txHash = pure . fst . (`urlParamsWithHash` config)

    processTx tx = processTxHandler `handleAny` case tx of
      InitPayment _ _ _ checkout _ -> do
        r <- pure $ parseRequest_ $ T.unpack $ "POST " <> (checkoutEndpoint config checkout)
        let req = addRequestHeader hContentType "application/x-www-form-urlencoded" $  setRequestBodyLBS (BS.pack $ T.unpack $ snd $ urlParamsWithHash tx config) r
        response <- httpLBS req
        let body = T.pack . BS.unpack $ getResponseBody response
        case getResponseStatusCode response of
          200 -> pure $ serverResponseToResult body
          _ -> pure $ Left $ ServerError
      PollPayment url -> do
        req <- pure $ parseRequest_ $ T.unpack $ "GET " <> url
        response <- httpLBS req
        case getResponseStatusCode response of
          200 -> 
            pure $ serverResponseToResult (T.pack . BS.unpack $ getResponseBody response)
          _ -> pure $ Left $ ServerError
      VerifyStatusUpdate _ -> 
        pure $ Left $ OtherError "not yet implemented"

    
    processTxHandler :: (MonadUnliftIO m) => SomeException -> m (Either PaynowError PaynowTXResult)
    processTxHandler ex = pure $ Left $ UnHandledException ex


serverResponseToResult :: Text -> Either PaynowError PaynowTXResult
serverResponseToResult resp = case ( sort $ M.keys kvp) of
--  InitExpressPaymentSuccess Status PaynowRef Instructions PollURL  Hash  
  ["hash", "instructions", "paynowreference", "pollurl", "status"] -> 
    Right $ InitExpressPaymentSuccess (ls "status") (l "paynowreference") (l "instructions") (l "pollurl") (l "hash")
--  InitClassicCheckoutSuccess Status RedirectURL PollURL Hash  
  ["browserurl", "hash", "pollurl", "status"] -> 
    Right $ InitClassicCheckoutSuccess (ls "status") (l "browserurl") (l "pollurl") (l "hash")
--  PollPaymentSuccess Status PaynowRef Ref Amount PollURL Hash  
  ["amount", "hash", "paynowreference", "pollurl", "reference", "status"] -> 
    Right $ PollPaymentSuccess (ls "status") (l "paynowreference") (l "reference") (la "amount") (l "pollurl") (l "hash")
  _ -> Left $ OtherError resp
  
  where
    l :: Text -> Text
    l k = fromMaybe "" $ M.lookup k kvp

    la :: Text -> Amount
    la k =  (read . T.unpack . fromMaybe "0" $ M.lookup k kvp) :: Amount

    ls :: Text -> Status
    ls t = case (T.toLower $ l t) of
      "paid" -> Paid
      "created" -> Created
      "ok" -> Okay
      "cancelled" -> Cancelled
      "failed" -> Failed
      "error" -> Error
      "refunded" -> Refunded
      other -> OtherStatus other
    

    kvp :: M.Map Text Text
    kvp = M.fromList $ (\t -> explode $ splitOn "=" t) <$> splitOn "&" resp

    explode :: [Text] -> (Text, Text)
    explode [] = ("", "")
    explode [k] = (k, "")
    explode [k, v] = (k, T.pack . urlDecode $ T.unpack v)
    explode (k : vs) = (k, T.pack . urlDecode $ T.unpack $ T.unwords vs)

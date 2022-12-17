# Paynow Zimbabwe Haskell SDK

Haskell SDK for Paynow Zimbabwe's API

# Prerequisites

This library has a set of prerequisites that must be met for it to work

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.Payments.Paynow as Paynow

	# Do stuff
```

---

# Usage example

```Haskell
defaultConfig :: PaynowConfig
defaultConfig = defaultProdConfig "1201"
                    "3e9fed89-60e1-4ce5-ab6e-6b1eb2d4f977"
                    "http://localhost:3000/result"
                    "http://localhost:3000/return"

....
client <- newPaynowClient config
payment <- newExpressCheckout (Ecocash "0779800700") "45" 37.50 "email" Nothing
result <- processTx payment
case result of
   Left e => //handle err
   Right r => //handle result r :: PaynowTxResult
```

Create a new payment passing in the reference for that payment (e.g invoice id, or anything that you can use to identify the transaction and the user's email address

---

> Express Transactions

```Haskell
testVMCDetails :: VMC
testVMCDetails = VMC (Card "4111111111111111" "588" "2023" "Trevor Sibanda") (BillingAddress "Address Line 1" "Line 2" "City" "Zimbabwe" Nothing)


payment <- newExpressCheckout client (OneMoney "0783102754") testRef testAmount testEmail Nothing

payment <- newExpressCheckout client (Ecocash "0715900800") testRef testAmount testEmail Nothing

payment <- newExpressCheckout client (VisaMastercard testVMCDetails) testRef testAmount testEmail Nothing

```

# Checking transaction status

The SDK exposes a handy method that you can use to check the status of a transaction. Once you have instantiated the Paynow class.

```Haskell
tx <- newPollTransaction "https://www.paynow.co.zw/Interface/CheckPayment/?guid=dece867e-5a40-4961-bf0e-5d691c2a97f8"
processtx tx >>= print
```

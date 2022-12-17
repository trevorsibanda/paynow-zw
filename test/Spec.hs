{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import          Test.Hspec

import           Data.Either
import UnliftIO
import Network.Payments.Paynow

-- | Run the tests for the paynow library
main :: IO ()
main = hspec $ do
  describe "PaynowTransaction" transactionSpec 
  describe "Transaction Hashing" hashingSpec
  describe "Response Parsing" responseSpec
  describe "Paynow Client" clientSpec

defaultConfig :: PaynowConfig
defaultConfig = defaultProdConfig "1201"
                    "3e9fed89-60e1-4ce5-ab6e-6b1eb2d4f977"
                    "http://localhost:3000/result"
                    "http://localhost:3000/return"

runWithDefaultClientConfig :: (MonadUnliftIO m) => (PaynowClient m -> PaynowConfig -> m a) -> m a
runWithDefaultClientConfig f = do
    f client config
    where
        config = defaultConfig
        client = newPaynowClient config

runWithDefaultClient :: (MonadUnliftIO m) => (PaynowClient m -> m a) -> m a
runWithDefaultClient f = runWithDefaultClientConfig (\cl _ -> f cl)


testRef :: Ref
testRef = "TEST REF"

testAmount :: Amount
testAmount = 99.99

testEmail :: Email
testEmail = "customer@gmail.com"

testVMCDetails :: VMC
testVMCDetails = VMC (Card "4111111111111111" "588" "2023" "Trevor Sibanda") (BillingAddress "Address Line 1" "Line 2" "City" "Zimbabwe" Nothing)

testPollResponse = "reference=01&paynowreference=15650881&amount=99.00&status=Cancelled&pollurl=https%3a%2f%2fwww.paynow.co.zw%2fInterface%2fCheckPayment%2f%3fguid%3dd80b830d-623b-4948-9ab1-aae4950929bd&hash=E4F6E55CF3408EBDF0F59951E9E7B74C17CD9798A8AE5DA7B5D68D04F014E40753C438AA55445EB42FD9E455EE7FFA7BF752AD6791DE878DAEF4B486A345EAC"

testClassicCheckoutResponse = "status=Ok&browserurl=https%3a%2f%2fwww.paynow.co.zw%2fPayment%2fConfirmPayment%2f15655543%2fsibandatrevor%40gmail.com%2f%2f&pollurl=https%3a%2f%2fwww.paynow.co.zw%2fInterface%2fCheckPayment%2f%3fguid%3ddece867e-5a40-4961-bf0e-5d691c2a97f8&hash=83B31644B176661DC643CEDD480ADF3C0DECC9E2A00B004B8DC37B6A88A2A246D1A3DE57D5BA41B61BBA2FE2580CEFDCDEE30698200B88CD85D62FE75E76CD4B"

testExpressCheckoutResponse = "status=Ok&instructions=Dial+*151*2*4%23+and+enter+your+EcoCash+PIN.+Once+you+have+authorized+the+payment+via+your+handset%2c+please+click+Check+For+Payment+below+to+conclude+this+transaction&paynowreference=15655549&pollurl=https%3a%2f%2fwww.paynow.co.zw%2fInterface%2fCheckPayment%2f%3fguid%3dc9f51998-3bbd-48b4-9107-819794c75ef8&hash=6DB1869C5C3961ADDF616B0476660E6D76E9071855ACAFA974F751A67600E0AD7416040518825BB235FF4AF3B57FD50A80884FB438DE8A2A6FAC62C71BECB51F"

clientSpec :: Spec
clientSpec = do
    it "should wrap exceptions in a PaynowError" $ do
        let client = newPaynowClient defaultConfig { pncEndpoint = "not-a-valid-url" }
        payment <- newClassicCheckout client testRef testAmount testEmail Nothing
        result <- processTx client payment
        result `shouldSatisfy` isLeft

responseSpec :: Spec
responseSpec = do
    it "should parse a successful express checkout response" $ do
        case serverResponseToResult testExpressCheckoutResponse of
            Right result -> 
                 result `shouldBe` InitExpressPaymentSuccess Okay "15655549" "Dial+*151*2*4#+and+enter+your+EcoCash+PIN.+Once+you+have+authorized+the+payment+via+your+handset,+please+click+Check+For+Payment+below+to+conclude+this+transaction" "https://www.paynow.co.zw/Interface/CheckPayment/?guid=c9f51998-3bbd-48b4-9107-819794c75ef8" "6DB1869C5C3961ADDF616B0476660E6D76E9071855ACAFA974F751A67600E0AD7416040518825BB235FF4AF3B57FD50A80884FB438DE8A2A6FAC62C71BECB51F"
            _ -> expectationFailure "Expected a successful poll response"  

    it "should parse a successful classic checkout response" $ do
        case serverResponseToResult testClassicCheckoutResponse of
            Right result -> 
                 result `shouldBe` InitClassicCheckoutSuccess Okay "https://www.paynow.co.zw/Payment/ConfirmPayment/15655543/sibandatrevor@gmail.com//" "https://www.paynow.co.zw/Interface/CheckPayment/?guid=dece867e-5a40-4961-bf0e-5d691c2a97f8" "83B31644B176661DC643CEDD480ADF3C0DECC9E2A00B004B8DC37B6A88A2A246D1A3DE57D5BA41B61BBA2FE2580CEFDCDEE30698200B88CD85D62FE75E76CD4B"
            _ -> expectationFailure "Expected a successful poll response"

    it "should successfully parse a poll response" $ do
        case serverResponseToResult testPollResponse of
            Right result -> 
                 result `shouldBe` PollPaymentSuccess Cancelled "15650881" "01"  99.00  "https://www.paynow.co.zw/Interface/CheckPayment/?guid=d80b830d-623b-4948-9ab1-aae4950929bd" "E4F6E55CF3408EBDF0F59951E9E7B74C17CD9798A8AE5DA7B5D68D04F014E40753C438AA55445EB42FD9E455EE7FFA7BF752AD6791DE878DAEF4B486A345EAC"
            _ -> expectationFailure "Expected a successful poll response"

    it "should fail gracefully on bad input" $ do
        serverResponseToResult "bad input" `shouldSatisfy` isLeft
        
  
transactionSpec :: Spec
transactionSpec = do

    it "should override the default return url" $ do
        runWithDefaultClientConfig $ \client config -> do
            let txdata = TxData Nothing Nothing (Just "https://api.com/override_return_url")
            payment <- newClassicCheckout client testRef testAmount testEmail (Just txdata)
            (snd $ urlParamsWithHash payment config) `shouldBe` "resulturl=http%3A%2F%2Flocalhost%3A3000%2Fresult&returnurl=https%3A%2F%2Fapi.com%2Foverride_return_url&reference=TEST%20REF&amount=99.99&id=1201&authemail=customer%40gmail.com&status=Message&hash=1C236C65D6DA213FE2F1F0B6472C6B15787CE1F5D9001ED698FEBDCC0FDFDBDFAD2310BDCB6ED8245E65BCA48D17E48F11666172BC894422DC3146582EBED2DC"

    it "should override the default result url" $ do
        runWithDefaultClientConfig $ \client config -> do
            let txdata = TxData Nothing (Just "https://api.com/override_result_url") Nothing
            payment <- newClassicCheckout client testRef testAmount testEmail (Just txdata)
            (snd $ urlParamsWithHash payment config) `shouldBe` "resulturl=https%3A%2F%2Fapi.com%2Foverride_result_url&returnurl=http%3A%2F%2Flocalhost%3A3000%2Freturn&reference=TEST%20REF&amount=99.99&id=1201&authemail=customer%40gmail.com&status=Message&hash=0AA2A87F4B0564341FCB47E9C3A0CCCECFE1E0F2A8DB6029081AB8D350CB189A631589279613B1A71EFA0511189C756368537EE717EC982BE8A1449AF44BC1D5"

    it "should include the additional info" $ do
        runWithDefaultClientConfig $ \client config -> do
            let txdata = TxData (Just "additional info to pass") Nothing Nothing
            payment <- newClassicCheckout client testRef testAmount testEmail (Just txdata)
            (snd $ urlParamsWithHash payment config) `shouldBe` "resulturl=http%3A%2F%2Flocalhost%3A3000%2Fresult&returnurl=http%3A%2F%2Flocalhost%3A3000%2Freturn&reference=TEST%20REF&amount=99.99&id=1201&additionalinfo=additional%20info%20to%20pass&authemail=customer%40gmail.com&status=Message&hash=2EE631E7F58FDCF10C3FC2602604DECE1E443A4F3A2D1FE05B9672C1A384FDA369FC4E529BDEDA30D651F974FFAFE9CDF494D2F176519F567E9675FE3804AEC1"
    


hashingSpec :: Spec
hashingSpec = do
    -- See https://developers.paynow.co.zw/docs/generating_hash.html        
    it "should generate sha512 hash correctly" $ do
        let payload = "1201TEST REF99.99A test ticket transactionhttp://www.google.com/search?q=returnurlhttp://www.google.com/search?q=resulturlMessage3e9fed89-60e1-4ce5-ab6e-6b1eb2d4f977"
            expectedHash = "2A033FC38798D913D42ECB786B9B19645ADEDBDE788862032F1BD82CF3B92DEF84F316385D5B40DBB35F1A4FD7D5BFE73835174136463CDD48C9366B0749C689"
        sha1Hex payload `shouldBe` expectedHash


    it "should generate a hash for a ClassicCheckout transaction" $ do
        runWithDefaultClient $ \client -> do
            payment <- newClassicCheckout client testRef testAmount testEmail Nothing
            hash <- txHash client payment
            hash `shouldBe` "79176952B0F3A8EA27B5FF22E1568B396A37017E65EFD1130A85175AB163C2894E5D97BC154184CE20CEC9C7B01456247FA7D942B583412DD42634579ABD9C33"

    it "should generate a hash for a Ecocash/ExpressCheckout transaction" $ do
        runWithDefaultClient $ \client -> do
            payment <- newExpressCheckout client (Ecocash "0783102754") testRef testAmount testEmail Nothing
            hash <- txHash client payment
            hash `shouldBe` "7FCBF1103BA1B6E6EA026DCE955200CEA23F69E42A023050457ED3E0B4489459FB4313ADD1446047F1AA9F3E0CE8E5FADCC3C4DAB386636A09DB560BD01506A3"

    it "should generate a hash for a OneMoney/ExpressCheckout transaction" $ do
        runWithDefaultClient $ \client -> do
            payment <- newExpressCheckout client (OneMoney "0783102754") testRef testAmount testEmail Nothing
            hash <- txHash client payment
            hash `shouldBe` "7E1770DC2A5BA3A9498B995F3096F9DCB83F6992816B7B1E418C0B1888ADF30252BF2175240209C53025E7025033202C6C07395707EF39C8B3349E136DE3CAEB"
 
    it "should generate a hash for a VMC/ExpressCheckout transaction" $ do
        runWithDefaultClient $ \client -> do
            payment <- newExpressCheckout client (VisaMastercard testVMCDetails) testRef testAmount testEmail Nothing
            hash <- txHash client payment
            hash `shouldBe` "79176952B0F3A8EA27B5FF22E1568B396A37017E65EFD1130A85175AB163C2894E5D97BC154184CE20CEC9C7B01456247FA7D942B583412DD42634579ABD9C33"

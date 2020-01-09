module Exchanges.Bilaxy.Tests (
  tests
) where

import           Control.Exception
import           Data.Proxy
import           Exchanges.Bilaxy.Exchange
import           Exchanges.Bilaxy.Query
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit
import           Types

assertThrows :: IO a -> IO ()
assertThrows action = do
  v <- try action
  case v of
    Left (SomeException _) -> return ()
    Right _                -> assertFailure "expected exception"

testPublic :: Test
testPublic = TestCase $ do
  r <- getTicker $ pairId (Proxy :: Proxy (TT,USDT,Bilaxy))
  print r -- not best way to force r but whatever

testPrivate :: Test
testPrivate = TestCase $ do
  b1 <- getBalanceOf (tokenName (Proxy :: Proxy TT))
  b2 <- getBalanceOf (tokenName (Proxy :: Proxy USDT))
  print (b1,b2) -- not best way to force b1/b2 but whatever

test_getBalance :: Test
test_getBalance = TestCase $ do
  b1 <- getBalance (Proxy :: Proxy (TT, Bilaxy))
  b2 <- getBalance (Proxy :: Proxy (USDT, Bilaxy))
  print (b1,b2) -- not best way to force b1/b2 but whatever

test_getOrderInfo :: Test
test_getOrderInfo = TestCase $
  assertThrows (getOrderInfo (-1))

test_getOrders :: Test
test_getOrders = TestCase $ do
  orders <- getOrders :: IO [Order TT USDT Bilaxy]
  print orders

test_getExchangeRate :: Test
test_getExchangeRate = TestCase $ do
  r <- getExchangeRate (Proxy :: Proxy (TT,USDT,Bilaxy))
  print r

-- yes this actually makes an order and cancel it...
-- uses a very very high sell price so unlikely to actually go through
test_order_cancel :: Test
test_order_cancel = TestCase $ do
  o <- order Sell (fromStdDenom 9) (fromStdDenom 123) :: IO (Order TT USDT Bilaxy)
  r <- cancel o
  print (o, r)


tests :: IO ()
tests = hspec $
  describe "Bilaxy" $ do
    describe "Public API" $
      fromHUnitTest testPublic
    describe "Private API" $
      fromHUnitTest testPrivate
    describe "getBalance" $
      fromHUnitTest test_getBalance
    describe "getOrderInfo" $
      fromHUnitTest test_getOrderInfo
    describe "getExchangeRate" $
      fromHUnitTest test_getExchangeRate
    describe "order_cancel" $
      fromHUnitTest test_order_cancel

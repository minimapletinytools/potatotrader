module Potato.Trader.Exchanges.Bilaxy.Tests (
  tests
) where

import           Control.Exception
import qualified Control.Monad.Catch                  as C
import           Control.Monad.Reader
import           Data.Proxy
import           Potato.Trader.Exchanges.Bilaxy
import           Potato.Trader.Exchanges.Bilaxy.Query
import           Potato.Trader.Types
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit             (fromHUnitTest)
import           Test.HUnit

bilaxyCtx :: (BilaxyData, BilaxyAccount)
bilaxyCtx = ((),nilKey)

assertThrows :: (MonadIO m, C.MonadCatch m) => m a -> m ()
assertThrows action = do
  v <- C.try action
  case v of
    Left (SomeException _) -> return ()
    Right _                -> liftIO $ assertFailure "expected exception"

-- test methods in Query.hs directly
testPublic :: Test
testPublic = TestCase $ do
  r <- getTicker $ pairId (Proxy :: Proxy (TT,USDT,Bilaxy))
  print r -- not best way to force r but whatever

testPrivate :: Test
testPrivate = TestCase $ do
  b1 <- getBalanceOf (tokenName (Proxy :: Proxy TT))
  b2 <- getBalanceOf (tokenName (Proxy :: Proxy USDT))
  print (b1,b2) -- not best way to force b1/b2 but whatever

-- test class methods in Exchange.hs
test_getBalance :: Test
test_getBalance = TestCase $ flip runReaderT bilaxyCtx $ do
  b1 <- getBalance (Proxy :: Proxy (TT,Bilaxy))
  b2 <- getBalance (Proxy :: Proxy (USDT,Bilaxy))
  liftIO $ print (b1,b2) -- not best way to force b1/b2 but whatever

test_getOrderInfo_fail :: Test
test_getOrderInfo_fail = TestCase $ flip runReaderT bilaxyCtx $
  assertThrows (liftIO $ getOrderInfo (-1))

test_getOrders :: Test
test_getOrders = TestCase $ flip runReaderT bilaxyCtx $ do
  orders <- getOrders (Proxy :: Proxy (TT,USDT,Bilaxy))
  liftIO $ print orders

test_getExchangeRate :: Test
test_getExchangeRate = TestCase $ flip runReaderT bilaxyCtx $ do
  r <- getExchangeRate (Proxy :: Proxy (TT,USDT,Bilaxy)) True
  liftIO $ print r

-- yes this actually makes an order and cancel it...
-- uses a very very high sell price so unlikely to actually go through
test_order_getStatus_cancel :: Test
test_order_getStatus_cancel = TestCase $ flip runReaderT bilaxyCtx $ do
  let p = (Proxy :: Proxy (TT,USDT,Bilaxy))
  -- Rigid enforces price by what we set and not based on asks
  o <- order p Rigid Sell (fromStdDenom 1000) (fromStdDenom 123)
  liftIO $ print o
  orders <- getOrders (Proxy :: Proxy (TT,USDT,Bilaxy))
  liftIO $ print orders
  s <- getStatus p o
  liftIO $ print s
  r <- cancel p o
  liftIO $ print r



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
      fromHUnitTest test_getOrderInfo_fail
    describe "getOrders" $
      fromHUnitTest test_getOrders
    describe "getExchangeRate" $
      fromHUnitTest test_getExchangeRate
    describe "order_cancel" $
      fromHUnitTest test_order_getStatus_cancel

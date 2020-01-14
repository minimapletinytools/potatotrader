module Potato.CryptoTrader.Exchanges.Chain.Tests (
  tests
) where

import           Control.Monad.Reader
import           Data.Proxy
import           Potato.CryptoTrader.Exchanges.Chain.Exchange
import           Potato.CryptoTrader.Exchanges.Chain.Query    hiding
                                                               (getBalance)
import           Potato.CryptoTrader.Types
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                     (fromHUnitTest)
import           Test.HUnit


type ChainReaderIO a = ReaderT (ChainCtx) IO a
chainCtx = ((),())

flipReaderT = flip runReaderT

test_getBalance :: Test
test_getBalance = TestCase $ flipReaderT chainCtx $ do
  b1 <- getBalance (Proxy :: Proxy (TT, OnChain TT))
  b2 <- getBalance (Proxy :: Proxy (USDT, OnChain TT))
  liftIO $ print (b1, b2) -- not best way to force but whatever

-- TODO delete this test once we have proper ExchangeAccount stuff done
test_getAddress :: Test
test_getAddress = TestCase $ flipReaderT chainCtx $ liftIO $ do
  r <- getAddress
  "0xc06Bd3dC3f6Ce518B55bDC469b1A8B81CBEaDc62" @?= r

test_getExchangeRate :: Test
test_getExchangeRate = TestCase $ flipReaderT chainCtx $ do
  r <- getExchangeRate (Proxy :: Proxy (TT,USDT,OnChain TT))
  liftIO $ print r

tests :: IO ()
tests = hspec $
  describe "Chain" $ do
    describe "getBalance" $
      fromHUnitTest test_getBalance
    describe "getExchangeRate" $
      fromHUnitTest test_getExchangeRate
    describe "getAddress" $
      fromHUnitTest test_getAddress

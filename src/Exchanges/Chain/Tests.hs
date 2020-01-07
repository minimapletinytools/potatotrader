module Exchanges.Chain.Tests (
  tests
) where

import           Data.Proxy
import           Exchanges.Chain.Exchange
import           Exchanges.Chain.Query    hiding (getBalance)
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit
import           Types

test_getBalance :: Test
test_getBalance = TestCase $ do
  b1 <- getBalance (Proxy :: Proxy (TT, OnChain ThunderCoreMain))
  b2 <- getBalance (Proxy :: Proxy (USDT, OnChain ThunderCoreMain))
  print (b1, b2) -- not best way to force but whatever

test_getAddress :: Test
test_getAddress = TestCase $ do
  r <- getAddress
  "0xc06Bd3dC3f6Ce518B55bDC469b1A8B81CBEaDc62" @?= r

tests :: IO ()
tests = hspec $
  describe "Chain" $ do
    describe "getBalance" $
      fromHUnitTest test_getBalance
    describe "Address" $
      fromHUnitTest test_getAddress

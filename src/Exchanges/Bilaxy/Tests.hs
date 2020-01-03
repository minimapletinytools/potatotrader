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
  r <- getTicker (pairID (Proxy :: Proxy (TT,USDT,Bilaxy)))
  print r -- not best way to force r but whatever

-- TODO update this to call getBalance via ExchangeToken typeclass
testPrivate :: Test
testPrivate = TestCase $ do
  r <- getBalanceOf (tokenName (Proxy :: Proxy TT))
  print r -- not best way to force r but whatever

test_getOrderInfo :: Test
test_getOrderInfo = TestCase $
  assertThrows (getOrderInfo (-1))

tests :: IO ()
tests = hspec $
  describe "Bilaxy" $ do
    describe "Public API" $
      fromHUnitTest testPublic
    describe "Private API" $
      fromHUnitTest testPrivate
    describe "getOrderInfo" $
      fromHUnitTest test_getOrderInfo

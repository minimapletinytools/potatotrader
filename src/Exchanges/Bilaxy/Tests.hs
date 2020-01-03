module Exchanges.Bilaxy.Tests (
  tests
) where

import           Data.Proxy
import           Exchanges.Bilaxy.Exchange
import           Exchanges.Bilaxy.Query
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit
import           Types

testPublic :: Test
testPublic = TestCase $ do
  r <- getTicker (pairID (Proxy :: Proxy (TT,USDT,Bilaxy)))
  print r -- not best way to force r but whatever

testPrivate :: Test
testPrivate = TestCase $ do
  r <- getBalanceOf (tokenName (Proxy :: Proxy TT))
  print r -- not best way to force r but whatever

tests :: IO ()
tests = hspec $
  describe "Bilaxy" $ do
    describe "Public API" $
      fromHUnitTest testPublic
    describe "Private API" $
      fromHUnitTest testPrivate

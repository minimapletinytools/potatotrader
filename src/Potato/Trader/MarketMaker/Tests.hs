{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

module Potato.Trader.MarketMaker.Tests (
  tests
) where

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit


-- TODO
--testCheckBuy
--testCheckSell

testMarketMaker :: Test
testMarketMaker = TestCase $ do
  -- TODO
  return ()

tests :: IO ()
tests = hspec $ do
  describe "MarketMaker" $
    fromHUnitTest testMarketMaker

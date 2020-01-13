module Potato.CryptoTrader.Tests (
  tests
) where

import           Data.Proxy
import           Potato.CryptoTrader.Helpers
import           Potato.CryptoTrader.Types
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit    (fromHUnitTest)
import           Test.HUnit


-- bids are people trying to buy TT
testBids :: [(AmountRatio USDT TT, Amount TT)]
testBids =
  [ (AmountRatio 100, Amount 100)
  , (AmountRatio 90, Amount 200)
  , (AmountRatio 80, Amount 100)]

-- asks are people trying to sell TT
testAsks :: [(AmountRatio USDT TT, Amount TT)]
testAsks =
  [ (AmountRatio 100, Amount 100)
  , (AmountRatio 110, Amount 200)
  , (AmountRatio 120, Amount 100)]

test_make_sellt1 :: Spec
test_make_sellt1 = do
  let
    mySellt1 = make_sellt1_from_bidst1 testBids
  it "returns expected value for boundary bids" $ do
    mySellt1 (Amount 100) `shouldBe` Amount (100*100)
    mySellt1 (Amount (100+200)) `shouldBe` Amount (100*100+200*90)
  it "returns correct value for partially executed bid" $ do
    mySellt1 (Amount (100+100)) `shouldBe` Amount (100*100+100*90)

test_make_buyt1 :: Spec
test_make_buyt1 = do
  let
    myBuyt1 = make_buyt1_from_askst1 testAsks
  it "returns expected value for boundary bids" $ do
    myBuyt1 (Amount 10000) `shouldBe` Amount (100)
    myBuyt1 (Amount (100*100+110*200)) `shouldBe` Amount (100+200)
  it "returns correct value for partially executed bid" $ do
    myBuyt1 (Amount (100*100+110*100)) `shouldBe` Amount (100+100)

tests :: IO ()
tests = hspec $
  describe "Bilaxy" $ do
    describe "make_sellt1" $
      test_make_sellt1
    describe "make_buyt1" $
      test_make_buyt1

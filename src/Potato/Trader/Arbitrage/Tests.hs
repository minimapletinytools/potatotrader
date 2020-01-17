{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

module Potato.Trader.Arbitrage.Tests (
  tests
) where

import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                         as T
import           Potato.Trader.Arbitrage
import           Potato.Trader.Exchanges.Bilaxy
import           Potato.Trader.Exchanges.Chain
import           Potato.Trader.ReverseExchangePair
import           Potato.Trader.Types
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit


testArbitrage :: Test
testArbitrage = TestCase $ do
  let
    ctx = (((),()),((),nilKey))
    params = ArbitrageParams {
        dryRun = True
        , minProfitAmount = (0,0)
      }
    p = Proxy :: Proxy (TT,USDT,OnChain ThunderCoreMain,Bilaxy)
    --p = Proxy :: Proxy (USDT,TT,ReverseExchangePair USDT TT (OnChain ThunderCoreMain),ReverseExchangePair USDT TT Bilaxy)
    arb = arbitrage p params
  (_,logs) <- runWriterT $ flip runReaderT ctx arb
  mapM_ (print . T.unpack) logs
  return ()


test_searchMax :: Spec
test_searchMax = do
  let
    res1 = [10,10,10]
    range1 = (0,100)
    f1_1 x = x
    f1_2 x = if x < 50 then x else 100-x
  it "returns right most value for f x = x" $
    (searchMax res1 range1 f1_1) `shouldBe` (100 :: Int, 100)
  it "returns correct max value for ^ looking function" $
    (searchMax res1 range1 f1_2) `shouldBe` (50 :: Int, 50)


tests :: IO ()
tests = hspec $ do
  describe "Arbitrage" $
    fromHUnitTest testArbitrage
  describe "searchMax" $
    test_searchMax

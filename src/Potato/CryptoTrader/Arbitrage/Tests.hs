{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

module Potato.CryptoTrader.Arbitrage.Tests (
  tests
) where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch                  as C
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Control.Monad.Trans                  as MTL
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import           Potato.CryptoTrader.Arbitrage
import           Potato.CryptoTrader.Exchanges.Bilaxy
import           Potato.CryptoTrader.Exchanges.Chain
import           Potato.CryptoTrader.Types
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit             (fromHUnitTest)
import           Test.HUnit


type E1 = (OnChain ThunderCoreMain)
type E2 = Bilaxy

type ArbMonad = ExchangePairT E1 E2 (WriterT ArbitrageLogs IO)
testArbitrage :: (ArbitrageConstraints TT USDT E1 E2 ArbMonad) =>
  Test
testArbitrage = TestCase $ do
  let
    ctx = (((),()),((),nilKey))
    arb = arbitrage (Proxy :: Proxy (TT,USDT,E1,E2))
  rslt <- runWriterT $ flip runReaderT ctx $ arb
  print rslt -- force rslt
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

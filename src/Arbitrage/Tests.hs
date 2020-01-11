{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

module Arbitrage.Tests (
  tests
) where

import           Arbitrage
import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch       as C
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Control.Monad.Trans       as MTL
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import           Exchanges.Bilaxy.Exchange
import           Exchanges.Chain.Exchange
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit
import           Types



type ArbMonad = ExchangePairT (OnChain ThunderCoreMain) Bilaxy (WriterT ArbitrageLogs IO)
testArbitrage :: forall t1 t2 e1 e2. (ArbitrageConstraints TT USDT (OnChain ThunderCoreMain) Bilaxy ArbMonad) =>
  Test
testArbitrage = TestCase $ do
  let
    ctx = (((),()),((),()))
    arb = doArbitrage (Proxy :: Proxy (TT,USDT,OnChain ThunderCoreMain, Bilaxy))
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
    f1_1 (searchMax res1 range1 f1_1) `shouldBe` (100 :: Int)
  it "returns correct max value for ^ looking function" $
    f1_2 (searchMax res1 range1 f1_2) `shouldBe` (50 :: Int)


tests :: IO ()
tests = hspec $ do
  --describe "Arbitrage" $
  --  fromHUnitTest testArbitrage
  describe "searchMax" $
    test_searchMax

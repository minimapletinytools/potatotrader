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


tests :: IO ()
tests = hspec $
  describe "Arbitrage" $
    fromHUnitTest testArbitrage

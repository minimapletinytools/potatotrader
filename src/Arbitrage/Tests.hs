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
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import           Exchanges.Bilaxy.Exchange
import           Exchanges.Chain.Exchange
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit
import           Types



type ArbMonad w = WriterT w (ReaderT (CtxPair (OnChain ThunderCoreMain) Bilaxy) IO)
testArbitrage :: forall t1 t2 e1 e2 w. (ArbitrageConstraints TT USDT (OnChain ThunderCoreMain) Bilaxy (ArbMonad w), Monoid w) =>
  Test
testArbitrage = TestCase $ do
  let
    ctx = CtxPair (((),()),((),())) :: CtxPair (OnChain ThunderCoreMain) Bilaxy
    arb :: WriterT w (ReaderT (CtxPair (OnChain ThunderCoreMain) Bilaxy) IO) ()
    arb = doArbitrage (Proxy :: Proxy (TT,USDT,OnChain ThunderCoreMain, Bilaxy))
  rslt <- flip runReaderT ctx $ runWriterT $ arb
  --print rslt -- force rslt
  return ()


tests :: IO ()
tests = hspec $
  describe "Arbitrage" $
    fromHUnitTest testArbitrage

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

module Potato.CryptoTrader.MarketMaker.Tests (
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
import qualified Data.Text                            as T
import           Potato.CryptoTrader.Arbitrage
import           Potato.CryptoTrader.Exchanges.Bilaxy
import           Potato.CryptoTrader.Exchanges.Chain
import           Potato.CryptoTrader.Types
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit             (fromHUnitTest)
import           Test.HUnit


type E1 = (OnChain ThunderCoreMain)
type E2 = Bilaxy

testMarketMaker :: Test
testMarketMaker = TestCase $ do undefined
  {-let
    ctx = (((),()),((),nilKey))
    arb = arbitrage (Proxy :: Proxy (TT,USDT,E1,E2)) True
  (_,logs) <- runWriterT $ flip runReaderT ctx arb
  mapM_ (print . T.unpack) logs
  return ()-}


tests :: IO ()
tests = hspec $ do
  describe "MarketMaker" $
    fromHUnitTest testMarketMaker

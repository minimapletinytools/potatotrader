{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

module Potato.Trader.MarketMaker.Tests (
  tests
) where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch            as C
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Control.Monad.Trans            as MTL
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                      as T
import           Potato.Trader.Exchanges.Bilaxy
import           Potato.Trader.Exchanges.Chain
import           Potato.Trader.MarketMaker
import           Potato.Trader.Types
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit       (fromHUnitTest)
import           Test.HUnit



defTTUSDTMarketMakerParams :: MarketMakerParams TT USDT
defTTUSDTMarketMakerParams = MarketMakerParams {
    orderMinMax = (fromStdDenom 150 :: Amount TT, fromStdDenom 150)
    --orderMinMax = (fromStdDenom 1000, fromStdDenom 3000)
    , minProfitMargin = AmountRatio (0.00075*3)
    , makerMargin = (AmountRatio 0.0007, AmountRatio 0.0007)
  }

testMarketMaker :: Test
testMarketMaker = TestCase $ do
  let
    ctx = ((),nilKey)
    mm = marketMaker (Proxy :: Proxy (TT,USDT,Bilaxy)) defTTUSDTMarketMakerParams
  (_,logs) <- runWriterT $ flip runReaderT ctx mm
  mapM_ (print . T.unpack) logs
  return ()

tests :: IO ()
tests = hspec $ do
  describe "MarketMaker" $
    fromHUnitTest testMarketMaker

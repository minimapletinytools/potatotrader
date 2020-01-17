{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Potato.CryptoTrader
import           Potato.CryptoTrader.Exchanges.Bilaxy
import           Potato.CryptoTrader.Exchanges.Chain

import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                            as T


type ArbMonad t1 t2 = ExchangePairT t1 t2 (WriterT ArbitrageLogs IO)
arbForever :: forall t1 t2 e1 e2. (ArbitrageConstraints t1 t2 e1 e2 (ArbMonad t1 t2))
  => Proxy (t1,t2,e1,e2)
  -> ArbitrageParameters t1 t2
  -> Amount t1
  -> ExchangeCtx e1
  -> ExchangeCtx e2
  -> IO ()
arbForever pproxy params profit ctx1 ctx2 = do
  -- run arbitrage
  putStrLn "=====BEGIN ARBITRAGE ROUND====="
  let
    ctx = (ctx1, ctx2)
    arb = arbitrage pproxy params
  (oss,logs) <- runWriterT $ runReaderT arb ctx
  mapM_ (print . T.unpack) logs
  osl <- flip runReaderT ctx $ do
    case oss of
      Nothing -> return Nothing
      Just (arbProfit, eo1, eo2) -> do
        osl1 <- lifte1 $ getStatus (Proxy :: Proxy (t1,t2,e1)) eo1
        osl2 <- lifte2 $ getStatus (Proxy :: Proxy (t1,t2,e2)) eo2
        return $ Just (arbProfit, osl1, osl2)
  newProfit <- case osl of
    Nothing -> print "no orders made" >> return profit
    Just (arbProfit, osl1, osl2) -> do
      putStrLn "=====ORDER STATUS====="
      print osl1
      print osl2
      -- in arbitrage, orders are expected to go through right away
      if orderState osl1 /= Executed || orderState osl1 /= Executed then do
        print "WARNING: order failed to execute"
        return profit
      else
        -- TODO check that expected profit amount matches reported profit from order status
        return $ profit + arbProfit
  putStrLn "=====END ARBITRAGE ROUND====="
  putStrLn $ "total profit is " ++ show (toStdDenom newProfit)
  putStrLn ""
  putStrLn ""
  putStrLn ""


  -- sleep for a minute
  threadDelay (floor 6e7)

  -- repeat
  arbForever pproxy params newProfit ctx1 ctx2


main :: IO ()
main = do
  let
    --p = Proxy :: Proxy (TT,USDT,OnChain ThunderCoreMain,Bilaxy)
    p = Proxy :: Proxy (USDT,TT,ReverseExchangePair USDT TT (OnChain ThunderCoreMain),ReverseExchangePair USDT TT Bilaxy)
    params = ArbitrageParameters {
        dryRun = True
        , minProfitAmount = (fromStdDenom 0.1 :: Amount USDT, fromStdDenom 30 :: Amount TT)
      }
  arbForever p params 0 ((),()) ((),nilKey)



--main = Potato.CryptoTrader.Exchanges.Bilaxy.recordDepth 151 10
--main = Potato.CryptoTrader.Exchanges.Bilaxy.testBalance
--main = Potato.CryptoTrader.Exchanges.Bilaxy.testDepth
--main = Potato.CryptoTrader.Exchanges.Chain.testTransaction

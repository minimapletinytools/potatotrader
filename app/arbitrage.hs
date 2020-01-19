{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Potato.Trader
import           Potato.Trader.Exchanges.Bilaxy
import           Potato.Trader.Exchanges.Chain

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                      as T


type ArbMonad e1 e2 = ExchangePairT e1 e2 (WriterT ArbitrageLogs IO)
arbForever :: forall t1 t2 e1 e2. (ArbitrageConstraints t1 t2 e1 e2 (ArbMonad e1 e2))
  => Proxy (t1,t2,e1,e2)
  -> ArbitrageParams t1 t2
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
  -- run arbitrage
  marbrslt <- try $ runWriterT $ runReaderT arb ctx

  (oss,logs) <- case marbrslt of
    Left (SomeException e) -> do
      putStrLn $ "arbitrage failed with exception: " ++ show e
      -- restart on failure
      threadDelay (floor 12e6)
      arbForever pproxy params profit ctx1 ctx2
      throwM e
    Right r -> return r
  mapM_ (print . T.unpack) logs
  -- query orders we just made
  osl <- flip runReaderT ctx $ do
    case oss of
      Nothing -> return Nothing
      Just (arbProfit, eo1, eo2) -> do
        osl1 <- lifte1 $ getStatus (Proxy :: Proxy (t1,t2,e1)) eo1
        osl2 <- lifte2 $ getStatus (Proxy :: Proxy (t1,t2,e2)) eo2
        return $ Just (arbProfit, osl1, osl2)
  -- report profit
  newProfit <- case osl of
    Nothing -> print "no orders made" >> return profit
    Just (arbProfit, osl1, osl2) -> do
      putStrLn "=====ORDER STATUS====="
      print osl1
      print osl2
      -- in arbitrage, orders are expected to go through right away
      if orderState osl1 /= Executed || orderState osl2 /= Executed then do
        print "WARNING: order failed to execute"
        return profit
      else
        -- TODO check that expected profit amount matches reported profit from order status
        return $ profit + arbProfit
  putStrLn "=====END ARBITRAGE ROUND====="
  putStrLn $ "total profit is " ++ show newProfit
  putStrLn ""
  putStrLn ""
  putStrLn ""


  -- sleep for a minute
  --threadDelay (floor 6e7)
  threadDelay (floor 12e6)

  -- repeat
  arbForever pproxy params newProfit ctx1 ctx2


main :: IO ()
main = do
  let
    --p = Proxy :: Proxy (TT,USDT,OnChain ThunderCoreMain,Bilaxy)
    p = Proxy :: Proxy (USDT,TT,ReverseExchangePair USDT TT (OnChain ThunderCoreMain),ReverseExchangePair USDT TT Bilaxy)
    params = ArbitrageParams {
        dryRun = False
        , minProfitAmount = (fromStdDenom 0.05 :: Amount USDT, fromStdDenom 15 :: Amount TT)
      }
  arbForever p params 0 ((),()) ((),nilKey)

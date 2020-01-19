{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

import           Control.Concurrent             (threadDelay)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Control.Monad.Trans            as MTL
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                      as T
import           Potato.Trader
import           Potato.Trader.Exchanges.Bilaxy
import           Potato.Trader.Exchanges.Chain


type MMMonad e = ExchangeT e (WriterT [T.Text] IO)
mmForever :: forall t1 t2 e. (MarketMakerConstraints t1 t2 e (MMMonad e))
  => Proxy (t1,t2,e)
  -> MarketMakerParams t1 t2
  -> ExchangeCtx e
  -> IO ()
mmForever pproxy params ctx = do
  putStrLn "=====BEGIN MARKET MAKER ROUND====="

  mmrslt <- try $ runWriterT $ flip runReaderT ctx $ marketMaker pproxy params

  (_,logs) <- case mmrslt of
    Left (SomeException e) -> do
      putStrLn $ "market maker failed with exception: " ++ show e
      -- restart on failure
      threadDelay (floor 12e6)
      mmForever pproxy params ctx
      throwM e
    Right r -> return r
  mapM_ (print . T.unpack) logs

  putStrLn "=====END MARKET MAKER ROUND====="
  --putStrLn $ "total profit is " ++ show newProfit
  putStrLn ""
  putStrLn ""
  putStrLn ""

  mmForever pproxy params ctx


main :: IO ()
main = do
  let
    defTTUSDTMarketMakerParams :: MarketMakerParams TT USDT
    defTTUSDTMarketMakerParams = MarketMakerParams {
        orderMinMax = (fromStdDenom 150 :: Amount TT, fromStdDenom 150)
        --orderMinMax = (fromStdDenom 1000, fromStdDenom 3000)
        , minProfitMargin = AmountRatio (0.00075*3)
        , makerMargin = (AmountRatio 0.0007, AmountRatio 0.0007)
      }
    ctx = ((),nilKey)
  mmForever (Proxy :: Proxy (TT,USDT,Bilaxy)) defTTUSDTMarketMakerParams ctx

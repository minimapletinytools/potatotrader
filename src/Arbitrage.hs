{-# LANGUAGE ConstraintKinds #-}

module Arbitrage (
  doArbitrage
) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import           Types

type ArbitrageConstraints t1 t2 e1 e2 = (Token t1, Token t2, Exchange e1, Exchange e2, ExchangePair t1 t2 e1, ExchangePair t1 t2 e2)


doArbitrage :: forall t1 t2 e1 e2 w m. (ArbitrageConstraints t1 t2 e1 e2, MonadIO m, MonadWriter w m) => Proxy (t1, t2, e1, e2) -> m ()
doArbitrage proxy = do

  -- query and cancel all orders
  qncresult <- liftIO . try $ do
    e1orders <- getOrders :: IO [Order t1 t2 e1]
    e2orders <- getOrders :: IO [Order t1 t2 e2]
    mapM_ cancel e1orders
    mapM_ cancel e2orders
  case qncresult of
    -- TODO log and error and restart
    Left (SomeException e) -> return ()
    Right _                -> return ()

  -- query balances
  gbresult <- liftIO . try $ do
    t1e1 <- getBalance (Proxy :: Proxy (t1, e1))
    t2e1 <- getBalance (Proxy :: Proxy (t2, e1))
    t1e2 <- getBalance (Proxy :: Proxy (t1, e2))
    t2e2 <- getBalance (Proxy :: Proxy (t2, e2))
    return (t1e1, t2e1, t1e2, t2e2)
  (t1e1, t2e1, t1e2, t2e2) <- case gbresult of
    -- TODO log and error and restart
    Left (SomeException e) -> return (0,0,0,0)
    Right r                -> return r

  -- query exchange rate
  erresult <- liftIO . try $ do
    er1 <- getExchangeRate (Proxy :: Proxy t1 t2 e1)
    er2 <- getExchangeRate (Proxy :: Proxy t1 t2 e2)
    return (re1, re2)
  (er1, er2) <- case erresult of
    -- TODO log and error and restart
    Left (SomeException e) -> undefined
    Right r -> return r

  let
    sellt1_e1 = sellt1 er1
    buyt1_e1 = buyt1 er1
    sellt1_e2 = sellt1 er2
    buyt1_e2 = buyt1 er2


  -- assume 0 tx fees for now
  -- arbitrage t1 means exchange t1->t2 in e1 and t2->t1 in e2
  profit_t1 = (buyt1_e2 (sellt1_e1 t1x) - t1x)
  -- arbitrage t2 means exchange t2->t1 in e1 and t1->t2 in e2
  profit_t2 =




    data ExchangeRate t1 t2 = ExchangeRate {
      -- | sellt1 returns approx amount of t2 bought for input of t1
      sellt1     :: Amount t1 -> Amount t2
      -- | buyt1 returns approx amount of t1 bought for input of t2
      , buyt1    :: Amount t2 -> Amount t1
      -- | variance returs the variance of the quantity |desired_t1/desired_t2 - actual_t1/actual_t2|
      -- does not distinguish between buy/sell
      -- TODO this should probably return something like (TimeDiff -> Double)
      , variance :: Amount t1 -> Amount t2 -> Double
    }

  -- check if arbitrage opportunity exists


  return ()

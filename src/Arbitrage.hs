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
    -- TODO log and error and fail silently
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
    -- TODO log and error and fail silently
    Left (SomeException e) -> return (0,0,0,0)
    Right r                -> return r

  -- query exchange rate

  -- check if arbitrage opportunity exists


  return ()

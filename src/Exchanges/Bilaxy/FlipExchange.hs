{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Exchanges.Bilaxy.FlipExchange (
  BilaxyFlip(..)
) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.List                 (mapAccumL)
import           Data.Proxy
import qualified Exchanges.Bilaxy.Aeson    as BA
import           Exchanges.Bilaxy.Exchange
import           Exchanges.Bilaxy.Query
import           Types

-- | Bilaxy exchange type where trading pairs are flipped
data BilaxyFlip

instance Exchange BilaxyFlip where
  exchangeName _ = "Bilaxy (flipped pairs)"
  type ExchangePairId BilaxyFlip = Int
  type ExchangeCache BilaxyFlip = BilaxyCache
  type ExchangeAccount BilaxyFlip = BilaxyAccount

instance ExchangeToken TT BilaxyFlip where
  getBalance _ = getBalance (Proxy :: Proxy (TT,Bilaxy))

instance ExchangeToken USDT BilaxyFlip where
  getBalance _ = getBalance (Proxy :: Proxy (USDT,Bilaxy))

flipProxy :: Proxy (t1, t2, BilaxyFlip) -> Proxy (t2, t1, Bilaxy)
flipProxy _ = Proxy

-- | current implementation of BilaxyFlip depends on ExchangePair t2 t1 Bilaxy and hence extra constraints
type BilaxyFlipExchangePairConstraints t1 t2 = (
  RealBilaxyPair t2 t1,
  ExchangeToken t1 BilaxyFlip,
  ExchangeToken t2 BilaxyFlip,
  ExchangeToken t1 Bilaxy,
  ExchangeToken t2 Bilaxy
  )

-- |
-- UNTESTED
instance (BilaxyFlipExchangePairConstraints t1 t2) => ExchangePair t1 t2 BilaxyFlip where
  -- uses same pairId as unflipped version
  pairId _ = getPairId (Proxy :: Proxy t2) (Proxy :: Proxy t1)

  type Order t1 t2 BilaxyFlip = BilaxyOrderDetails

  getStatus p = getStatus (flipProxy p)

  canCancel p = canCancel (flipProxy p)

  cancel p = cancel (flipProxy p)

  -- Note that this returns Bilaxy (not flip) orders too
  getOrders _ = do
    orders <- liftIO $ getOrderList $ pairId (Proxy :: Proxy (t1, t2, BilaxyFlip))
    return $ map (BilaxyOrderDetails . BA.oi_id) orders

  order _ ot (Amount t1) (Amount t2) = do
    let
      t1proxy = Proxy :: Proxy t1
      t2proxy = Proxy :: Proxy t2
      pproxy = Proxy :: Proxy (t2, t1, Bilaxy)
      amount_t1 = fromIntegral t1 / fromIntegral (decimals t1proxy)
      amount_t2 = fromIntegral t2 / fromIntegral (decimals t2proxy)
      price_t2 = amount_t2 / amount_t1
      pair = pairId pproxy
      ot' = if ot == Buy then Sell else Buy
    v <- liftIO $ try (postOrder pair amount_t2 amount_t1 ot)
    case v of
      Left (SomeException e) -> do
        liftIO $ print e
        return undefined
      Right oid              -> return $ BilaxyOrderDetails oid

  -- TODO test, not totally sure it's correct...
  getExchangeRate _ = do
    er <- getExchangeRate (Proxy :: Proxy (t2,t1,Bilaxy))
    return $ ExchangeRate (buyt1 er) (sellt1 er) (flip $ variance er)

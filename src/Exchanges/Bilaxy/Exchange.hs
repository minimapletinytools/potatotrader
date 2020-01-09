{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Exchanges.Bilaxy.Exchange (
  Bilaxy(..),
  BilaxyOrderDetails(..)
) where

import           Control.Exception
import           Data.List              (mapAccumL)
import           Data.Proxy
import qualified Exchanges.Bilaxy.Aeson as BA
import           Exchanges.Bilaxy.Query
import           Types


class (Token t1, Token t2) => RealBilaxyPair t1 t2 where
  getPairId :: Proxy t1 -> Proxy t2 -> Int

instance RealBilaxyPair TT USDT where
  getPairId _ _ = 151

data Bilaxy

instance Exchange Bilaxy where
  exchangeName _ = "Bilaxy"
  type ExchangePairId Bilaxy = Int

data BilaxyFlip

instance Exchange BilaxyFlip where
  exchangeName _ = "Bilaxy (flipped pairs)"
  type ExchangePairId BilaxyFlip = Int


getBalanceHelper :: forall t e. (Token t, ExchangeToken t e) => Proxy (t,e) -> IO (Amount t)
getBalanceHelper p = do
  b <- getBalanceOf $ symbol p
  return . Amount . floor $ fromIntegral (decimals (Proxy :: Proxy t)) * b

instance ExchangeToken TT Bilaxy where
  getBalance = getBalanceHelper

instance ExchangeToken USDT Bilaxy where
  getBalance = getBalanceHelper

instance ExchangeToken TT BilaxyFlip where
  getBalance = getBalanceHelper

instance ExchangeToken USDT BilaxyFlip where
  getBalance = getBalanceHelper

data BilaxyOrderDetails = BilaxyOrderDetails {
  orderId :: Int
} deriving (Show)

type BilaxyExchangePairConstraints t1 t2 = (Token t1, Token t2, RealBilaxyPair t1 t2, ExchangeToken t1 Bilaxy, ExchangeToken t2 Bilaxy)

instance BilaxyExchangePairConstraints t1 t2 => ExchangePair t1 t2 Bilaxy where
  pairId _ = getPairId (Proxy :: Proxy t1) (Proxy :: Proxy t2)

  -- TODO finish... Could include exchange pair id but it's encoded in the type so idk :\
  type Order t1 t2 Bilaxy = BilaxyOrderDetails

  getStatus _ (BilaxyOrderDetails oid) = do
    v <- try (getOrderInfo oid)
    case v of
      Left (SomeException _) -> return $ OrderStatus Missing
      Right oi               -> return . OrderStatus . BA.toOrderState . BA.oi_status $ oi

  canCancel _ _ = True

  cancel _ (BilaxyOrderDetails oid) = do
    v <- try (cancelOrder oid)
    case v of
      Left (SomeException _) -> return False
      Right oi               -> return True

  getOrders _ = do
    orders <- getOrderList $ pairId (Proxy :: Proxy (t1, t2, Bilaxy))
    return $ map (BilaxyOrderDetails . BA.oi_id) orders

  order _ ot (Amount t1) (Amount t2) = do
    let
      t1proxy = Proxy :: Proxy t1
      t2proxy = Proxy :: Proxy t2
      pproxy = Proxy :: Proxy (t1, t2, Bilaxy)
      amount_t1 = fromIntegral t1 / fromIntegral (decimals t1proxy)
      amount_t2 = fromIntegral t2 / fromIntegral (decimals t2proxy)
      price_t2 = amount_t2 / amount_t1
      pair = pairId pproxy
    v <- try (postOrder pair amount_t1 amount_t2 ot)
    case v of
      Left (SomeException e) -> do
        print e
        return undefined
      Right oid              -> return $ BilaxyOrderDetails oid

  getExchangeRate :: Proxy (t1,t2,Bilaxy) -> IO (ExchangeRate t1 t2)
  getExchangeRate pproxy = do
    let
      pair = pairId pproxy
    depth <- getDepth pair
    let
      t1d = fromInteger $ decimals (Proxy :: Proxy t1)
      t2d = fromInteger $ decimals (Proxy :: Proxy t2)
      -- TODO check this is correct
      -- asks are people trying to sell t2 for t1
      asks = map (\(BA.MarketOrder p v _) -> (ceiling $ p*t2d, floor $ v*t1d)) $ BA.asks depth
      -- bids are people trying to buy t2 with t1
      bids = BA.bids depth
    let
      -- TODO test
      sellt1 (Amount t1) = Amount $ r where
        func :: Integer -> (Integer, Integer) -> (Integer, Integer)
        func remainingt1 (price, volume) = (remainingt1-paidt1, boughtt2) where
          boughtt2 = min (remainingt1 `div` price) volume
          paidt1 = price * boughtt2
        (remaining, boughtt2Array) = mapAccumL func t1 asks
        -- TODO log a warning if remaining > 0 (means we bought the whole market and had some left over)
        boughtt2Executed = takeWhile (> 0) boughtt2Array
        r = foldl (+) 0 boughtt2Executed
      -- TODO
      buyt1 (Amount t2) = Amount $ 0
      variance = undefined
    return $ ExchangeRate sellt1 buyt1 variance

flipProxy :: Proxy (t1, t2, BilaxyFlip) -> Proxy (t2, t1, Bilaxy)
flipProxy _ = Proxy

-- current implementation depends on ExchangePair t2 t1 Bilaxy and hence extra constraints
type BilaxyFlipExchangePairConstraints t1 t2 = (Token t1, Token t2, RealBilaxyPair t2 t1, ExchangeToken t1 BilaxyFlip, ExchangeToken t2 BilaxyFlip, ExchangeToken t1 Bilaxy, ExchangeToken t2 Bilaxy)

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
    orders <- getOrderList $ pairId (Proxy :: Proxy (t1, t2, BilaxyFlip))
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
    v <- try (postOrder pair amount_t2 amount_t1 ot)
    case v of
      Left (SomeException e) -> do
        print e
        return undefined
      Right oid              -> return $ BilaxyOrderDetails oid

  -- TODO test, not totally sure it's correct...
  getExchangeRate :: Proxy (t1,t2,BilaxyFlip) -> IO (ExchangeRate t1 t2)
  getExchangeRate _ = do
    er <- getExchangeRate (Proxy :: Proxy (t2,t1,Bilaxy))
    return $ ExchangeRate (buyt1 er) (sellt1 er) (flip $ variance er)

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Exchanges.Bilaxy.Exchange (
  Bilaxy(..),
  BilaxyOrderDetails(..)
) where

import           Control.Exception
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
  data Order t1 t2 Bilaxy = BilaxyOrder BilaxyOrderDetails deriving (Show)
  getStatus (BilaxyOrder (BilaxyOrderDetails oid)) = do
    v <- try (getOrderInfo oid)
    case v of
      Left (SomeException _) -> return $ OrderStatus Missing
      Right oi               -> return . OrderStatus . BA.toOrderState . BA.oi_status $ oi
  canCancel _ = True
  cancel (BilaxyOrder (BilaxyOrderDetails oid)) = do
    v <- try (cancelOrder oid)
    case v of
      Left (SomeException _) -> return False
      Right oi               -> return True
  order :: (BilaxyExchangePairConstraints t1 t2) => OrderType -> Amount t1 -> Amount t2 -> IO (Order t1 t2 Bilaxy)
  order ot (Amount t1) (Amount t2) = do
    let
      -- TODO generalize this conversion function
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
      Right oid              -> return $ BilaxyOrder (BilaxyOrderDetails oid)

type BilaxyFlipExchangePairConstraints t1 t2 = (Token t1, Token t2, RealBilaxyPair t2 t1, ExchangeToken t1 BilaxyFlip, ExchangeToken t2 BilaxyFlip)

-- |
instance (BilaxyFlipExchangePairConstraints t1 t2) => ExchangePair t1 t2 BilaxyFlip where
  -- uses same pairId as unflipped version
  pairId _ = getPairId (Proxy :: Proxy t2) (Proxy :: Proxy t1)
  data Order t1 t2 BilaxyFlip = BilaxyFlipOrder BilaxyOrderDetails deriving (Show)
  getStatus _ = undefined
  canCancel _ = True
  cancel _ = undefined
  order :: (BilaxyFlipExchangePairConstraints t1 t2) => OrderType -> Amount t1 -> Amount t2 -> IO (Order t1 t2 BilaxyFlip)
  order ot (Amount t1) (Amount t2) = undefined

{-# LANGUAGE TypeFamilies #-}
module Exchanges.Bilaxy.Exchange (
  Bilaxy(..),
  unBilaxyPair
) where

import           Control.Exception
import           Data.Proxy
import qualified Exchanges.Bilaxy.Aeson as BA
import           Exchanges.Bilaxy.Query
import           Types

data Bilaxy

instance Exchange Bilaxy where
  exchangeName _ = "Bilaxy"
  data ExchangePairId Bilaxy = BilaxyTradingPair Int

unBilaxyPair :: ExchangePairId Bilaxy -> Int
unBilaxyPair (BilaxyTradingPair pair) = pair

eighteenDecimals :: Integer
eighteenDecimals = read "1000000000000000000"
sixDecimals :: Integer
sixDecimals = read "1000000"

getBalanceHelper :: (ExchangeToken t e) => Proxy (t,e) -> Integer -> IO (Amount t)
getBalanceHelper p decimals = do
  b <- getBalanceOf $ symbol p
  return . Amount . floor $ fromIntegral decimals * b

instance ExchangeToken TT Bilaxy where
  decimals _ = eighteenDecimals
  getBalance p = getBalanceHelper p eighteenDecimals

instance ExchangeToken USDT Bilaxy where
  decimals _ = sixDecimals
  getBalance p = getBalanceHelper p sixDecimals

instance ExchangePair TT USDT Bilaxy where
  pairId _ = BilaxyTradingPair 151
  -- TODO finish... Could include exchange pair id but it's encoded in the type so idk :\
  data Order TT USDT Bilaxy = BilaxyOrder {
    orderId :: Int
  }
  getStatus (BilaxyOrder oid) = do
    v <- try (getOrderInfo oid)
    case v of
      Left (SomeException _) -> return $ OrderStatus Missing
      Right oi               -> return . OrderStatus . BA.toOrderState . BA.oi_status $ oi
  canCancel _ = True
  cancel (BilaxyOrder oid) = do
    v <- try (cancelOrder oid)
    case v of
      Left (SomeException _) -> return False
      Right oi               -> return True
  order :: OrderType -> Amount TT -> Amount USDT -> IO (Order TT USDT Bilaxy)
  order ot tt usdt = do
    let
      -- TODO generalize this conversion function
      etproxy = Proxy :: Proxy (TT, Bilaxy)
      pproxy = Proxy :: Proxy (TT,USDT,Bilaxy)
      amount_tt = fromIntegral tt / fromIntegral (decimals etproxy)
      amount_usdt = fromIntegral usdt / fromIntegral (decimals etproxy)
      price_usdt = amount_usdt / amount_tt
      BilaxyTradingPair pair = pairId pproxy
    v <- try (postOrder pair amount_tt amount_usdt ot)
    case v of
      Left (SomeException _) -> undefined
      Right oid              -> return $ BilaxyOrder oid

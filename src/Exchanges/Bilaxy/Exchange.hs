{-# LANGUAGE TypeFamilies #-}
module Exchanges.Bilaxy.Exchange (
  Bilaxy(..)
) where

import           Data.Proxy
import           Exchanges.Bilaxy.Query
import           Types


data Bilaxy

instance Exchange Bilaxy where
  exchangeName _ = "Bilaxy"


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
  data Order TT USDT = OrderTTUSDT {
    id :: Integer
  }
  pairID _ = 151
  getStatus :: Order TT USDT -> IO OrderStatus
  getStatus order = undefined
  canCancel _ = True
  cancel _ = undefined

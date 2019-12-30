{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Exchanges (
) where

import TokenTypes
import Data.Proxy
import Data.Solidity.Prim.Address (Address)

-- tokens
data TT
data ETH
data USDT

instance Token TT where
  tokenName _ = "TT"

-- networks
data ThunderCoreMain

instance Network ThunderCoreMain where
  networkName _ = "ThunderCore mainnet"
  rpc _ = "https://mainnet-rpc.thundercore.com"

-- exchanges
data OnChain n = OnChain

instance (Network n) => Exchange (OnChain n) where
  exchangeName _ = networkName (Proxy :: Proxy n)

data Bilaxy

instance Exchange Bilaxy where
  exchangeName _ = "Bilaxy"

-- Token Exchanges
instance Exchange (OnChain n) => ExchangeToken TT (OnChain n) where
  --symbol _ = "ETH<->TT" ++ exchangeName (Proxy :: Proxy (Uniswap n))
  --ethAddr _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"
  getBalance _ = undefined

instance ExchangeToken TT Bilaxy where
  symbol _ = "TT"
  decimals _ = read "1000000000000000000"
  getBalance _ = undefined

-- Token ExchangePairs

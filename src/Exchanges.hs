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

instance Token TT where
  tokenName _ = "TT"

-- networks
data ThunderCoreMain

instance Network ThunderCoreMain where
  networkName _ = "ThunderCore mainnet"
  rpc _ = "https://mainnet-rpc.thundercore.com"

-- exchanges
data Uniswap n = Uniswap

instance (Network n) => Exchange (Uniswap n) where
  exchangeName _ = "Uniswap-" ++ networkName (Proxy :: Proxy n)

instance Exchange (Uniswap n) => ExchangeToken TT (Uniswap n) where
  symbol _ = "ETH<->TT" ++ exchangeName (Proxy :: Proxy (Uniswap n))
  ethAddr _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"
  getBalance _ = undefined

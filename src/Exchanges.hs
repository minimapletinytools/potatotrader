{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Exchanges (
) where

import           Data.Proxy
import           Data.Solidity.Prim.Address (Address)
import           Types

-- Token Exchanges
instance Exchange (OnChain n) => ExchangeToken TT (OnChain n) where
  --symbol _ = "ETH<->TT" ++ exchangeName (Proxy :: Proxy (Uniswap n))
  --ethAddr _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"
  getBalance _ = undefined


-- Token ExchangePairs

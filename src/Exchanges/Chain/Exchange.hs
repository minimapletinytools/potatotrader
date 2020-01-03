{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Exchanges.Chain.Exchange (
  ThunderCoreMain(..),
  EthereumMain(..),

  OnChain(..)
) where

import           Data.Proxy
import           Data.Solidity.Prim.Address (Address)
import qualified Exchanges.Chain.Query      as Q
import           Types

eighteenDecimals :: Integer
eighteenDecimals = read "1000000000000000000"

-- networks
data ThunderCoreMain
data EthereumMain

instance Network ThunderCoreMain where
  networkName _ = "ThunderCore mainnet"
  rpc _ = "https://mainnet-rpc.thundercore.com"
  chainID _ = 108

instance Network EthereumMain where
  networkName _ = "Ethereum mainnet"
  -- my private RPC url -__- don't use
  rpc _ = "https://mainnet.infura.io/v3/2edbdd953f714eeab3f0001bb0b96b91"
  chainID _ = 1

data OnChain n

instance (Network n) => Exchange (OnChain n) where
  exchangeName _ = networkName (Proxy :: Proxy n)

-- Token Exchanges
instance (Exchange (OnChain n), Network n) => ExchangeToken TT (OnChain n) where
  decimals _ = eighteenDecimals
  getBalance _ = Amount <$> Q.getBalance (rpc p) where
    p = Proxy :: Proxy n

ttUSDT = "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"
instance (Exchange (OnChain n), Network n) => ExchangeToken USDT (OnChain n) where
  decimals _ = eighteenDecimals
  getBalance _ = Amount <$> Q.getTokenBalance (rpc p) ttUSDT where
    p = Proxy :: Proxy n


-- Token ExchangePairs

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Exchanges.Chain.Exchange (
  ThunderCoreMain(..),
  EthereumMain(..),

  OnChain(..)
) where

import           Control.Exception
import           Data.Proxy
import           Data.Solidity.Prim.Address (Address)
import qualified Exchanges.Chain.Query      as Q
import           Network.Ethereum.Api.Types (TxReceipt (..))
import           Types


class Network n where
  networkName :: Proxy n -> String
  rpc :: Proxy n -> String
  chainId :: Proxy n -> Integer

-- networks
data ThunderCoreMain
data EthereumMain

instance Network ThunderCoreMain where
  networkName _ = "ThunderCore mainnet"
  rpc _ = "https://mainnet-rpc.thundercore.com"
  chainId _ = 108

instance Network EthereumMain where
  networkName _ = "Ethereum mainnet"
  -- my private RPC url -__- don't use
  rpc _ = "https://mainnet.infura.io/v3/2edbdd953f714eeab3f0001bb0b96b91"
  chainId _ = 1

data OnChain n

instance (Network n) => Exchange (OnChain n) where
  exchangeName _ = networkName (Proxy :: Proxy n)
  type ExchangePairId (OnChain n) = Address

-- Token Exchanges
instance (Exchange (OnChain n), Network n) => ExchangeToken TT (OnChain n) where
  getBalance _ = Amount <$> Q.getBalance (rpc p) where
    p = Proxy :: Proxy n

ttUSDT = "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"
instance (Exchange (OnChain n), Network n) => ExchangeToken USDT (OnChain n) where
  getBalance _ = Amount <$> Q.getTokenBalance (rpc p) ttUSDT where
    p = Proxy :: Proxy n


instance (Exchange (OnChain n), Network n) => ExchangePair TT USDT (OnChain n) where
  pairId _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"
  data Order TT USDT (OnChain n) = OnChainOrder {
    receipt :: TxReceipt
  }
  getStatus (OnChainOrder receipt) = do
    let url = rpc (Proxy :: Proxy n)
    v <- try (Q.getTransactionByHash url $ receiptTransactionHash receipt)
    case v of
      Left (SomeException _) -> return $ OrderStatus Missing
      Right _                -> return $ OrderStatus Executed
  canCancel _ = False
  order :: OrderType -> Amount TT -> Amount USDT -> IO (Order TT USDT (OnChain n))
  order ot (Amount tt) (Amount usdt) = do
    let
      nproxy = Proxy :: Proxy n
      pproxy = Proxy :: Proxy (TT,USDT,OnChain n)
      addr = pairId pproxy
      cid = chainId nproxy
      url = rpc nproxy
    v <- case ot of
      Buy  -> try (Q.txEthToTokenSwap url cid addr (fromIntegral tt) (fromIntegral usdt))
      Sell -> undefined -- TODO
    case v of
      Left (SomeException _) -> undefined
      Right receipt          -> return $ OnChainOrder receipt

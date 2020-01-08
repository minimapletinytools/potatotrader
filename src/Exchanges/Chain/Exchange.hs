{-# LANGUAGE ConstraintKinds       #-}
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

-- network
class Network n where
  networkName :: Proxy n -> String
  rpc :: Proxy n -> String
  chainId :: Proxy n -> Integer

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

-- helpers
class (Token t, Network n) => ChainToken t n where
  tokenAddress :: Proxy(t,n) -> Maybe Address
  tokenAddress _ = Nothing

instance ChainToken TT ThunderCoreMain

instance ChainToken USDT ThunderCoreMain where
  tokenAddress _ = Just "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"

class (Token t1, Token t2, Network n) => Uniswap t1 t2 n where
  uniswapAddress :: Proxy (t1,t2,n) -> Address

instance Uniswap TT USDT ThunderCoreMain where
  uniswapAddress _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"

-- exchange
data OnChain n

instance (Network n) => Exchange (OnChain n) where
  exchangeName _ = networkName (Proxy :: Proxy n)
  type ExchangePairId (OnChain n) = Address

-- Token Exchanges
type ExchangeTokenConstraint t n = (Token t, Exchange (OnChain n), Network n, ChainToken t n)
instance ExchangeTokenConstraint t n => ExchangeToken t (OnChain n) where
  getBalance _ = let url = rpc (Proxy :: Proxy n) in
    case tokenAddress (Proxy :: Proxy (t,n)) of
      Just addr -> Amount <$> Q.getTokenBalance url addr
      Nothing   ->  Amount <$> Q.getBalance url


type ExchangePairConstraint t1 t2 n = (ExchangeTokenConstraint t1 n, ExchangeTokenConstraint t2 n, Uniswap t1 t2 n)
instance ExchangePairConstraint t1 t2 n => ExchangePair t1 t2 (OnChain n) where
  pairId _ = uniswapAddress (Proxy :: Proxy(t1,t2,n))

  data Order t1 t2 (OnChain n) = OnChainOrder {
    receipt :: TxReceipt
  }

  getStatus (OnChainOrder receipt) = do
    let url = rpc (Proxy :: Proxy n)
    v <- try (Q.getTransactionByHash url $ receiptTransactionHash receipt)
    case v of
      Left (SomeException _) -> return $ OrderStatus Missing
      Right _                -> return $ OrderStatus Executed

  canCancel _ = False

  order :: OrderType -> Amount t1 -> Amount t2 -> IO (Order t1 t2 (OnChain n))
  order ot (Amount tt) (Amount usdt) = do
    let
      nproxy = Proxy :: Proxy n
      pproxy = Proxy :: Proxy (t1,t2,OnChain n)
      addr = pairId pproxy
      cid = chainId nproxy
      url = rpc nproxy
    v <- case ot of
      Buy  -> try (Q.txEthToTokenSwap url cid addr (fromIntegral tt) (fromIntegral usdt))
      Sell -> undefined -- TODO
    case v of
      Left (SomeException _) -> undefined
      Right receipt          -> return $ OnChainOrder receipt

  getExchangeRate :: Proxy (t1,t2,OnChain n) -> IO (ExchangeRate t1 t2)
  getExchangeRate _ = undefined

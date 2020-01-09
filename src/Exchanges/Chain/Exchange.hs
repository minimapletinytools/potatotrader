{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


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

type family BaseToken t where
  BaseToken TT = 'True
  BaseToken USDT = 'False

class (Token t1, Token t2) => Uniswap (flag :: Bool) t1 t2 where
  -- |
  -- exchanges t1 for t2 where input amount (t1) is fixed
  t1t2SwapInput :: Proxy flag -> String -> Integer -> Address -> Amount t1 -> Amount t2 -> IO TxReceipt

instance (Token t1, Token t2) => Uniswap 'True t1 t2 where
  t1t2SwapInput _ url cid addr (Amount input_t1) (Amount min_t2) = Q.txEthToTokenSwapInput url cid addr (fromIntegral input_t1) (fromIntegral min_t2)

instance (Token t1, Token t2) => Uniswap 'False t1 t2 where
  t1t2SwapInput _ url cid addr (Amount input_t1) (Amount min_t2) = Q.txTokenToEthSwapInput url cid addr (fromIntegral input_t1) (fromIntegral min_t2)

class (Token t1, Token t2, Network n) => UniswapNetwork t1 t2 n where
  uniswapAddress :: Proxy (t1,t2,n) -> Address

instance UniswapNetwork TT USDT ThunderCoreMain where
  uniswapAddress _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"

getBalanceOf :: forall t n. (ChainToken t n, Network n) => Proxy (t,n) -> Address -> IO (Amount t)
getBalanceOf _ acct = let url = rpc (Proxy :: Proxy n) in
  case tokenAddress (Proxy :: Proxy (t,n)) of
    Just addr -> Amount <$> Q.getTokenBalanceOf url addr acct
    Nothing   ->  Amount <$> Q.getBalanceOf url acct

-- exchange
data OnChain n

instance (Network n) => Exchange (OnChain n) where
  exchangeName _ = networkName (Proxy :: Proxy n)
  type ExchangePairId (OnChain n) = Address

-- Token Exchanges
data OnChainOrder = OnChainOrder {
  receipt :: TxReceipt
}

type ExchangeTokenConstraint t n = (Token t, Exchange (OnChain n), Network n, ChainToken t n)
instance ExchangeTokenConstraint t n => ExchangeToken t (OnChain n) where
  getBalance _ = let url = rpc (Proxy :: Proxy n) in
     case tokenAddress (Proxy :: Proxy (t,n)) of
       Just addr -> Amount <$> Q.getTokenBalance url addr
       Nothing   ->  Amount <$> Q.getBalance url

type ExchangePairConstraint t1 t2 n = (Network n, ExchangeTokenConstraint t1 n, ExchangeTokenConstraint t2 n, Uniswap (BaseToken t1) t1 t2, Uniswap (BaseToken t2) t2 t1, UniswapNetwork t1 t2 n)
instance ExchangePairConstraint t1 t2 n => ExchangePair t1 t2 (OnChain n) where
  pairId _ = uniswapAddress (Proxy :: Proxy(t1,t2,n))

  type Order t1 t2 (OnChain n) = OnChainOrder

  getStatus _ (OnChainOrder receipt) = do
    let url = rpc (Proxy :: Proxy n)
    v <- try (Q.getTransactionByHash url $ receiptTransactionHash receipt)
    case v of
      Left (SomeException _) -> return $ OrderStatus Missing
      Right _                -> return $ OrderStatus Executed

  -- TODO TEST
  order _ ot t1 t2 = do
    let
      nproxy = Proxy :: Proxy n
      pproxy = Proxy :: Proxy (t1,t2,OnChain n)
      addr = pairId pproxy
      cid = chainId nproxy
      url = rpc nproxy
    v <- case ot of
      Buy  -> try (t1t2SwapInput (Proxy :: Proxy (BaseToken t1)) url cid addr t1 t2)
      Sell -> try (t1t2SwapInput (Proxy :: Proxy (BaseToken t2)) url cid addr t2 t1)
    case v of
      Left (SomeException _) -> undefined
      Right receipt          -> return $ OnChainOrder receipt

  -- TODO test
  getExchangeRate :: Proxy (t1,t2,OnChain n) -> IO (ExchangeRate t1 t2)
  getExchangeRate pnproxy = do
    let
      nproxy = Proxy :: Proxy n
      t1nproxy = Proxy :: Proxy (t1,n)
      t2nproxy = Proxy :: Proxy (t2,n)
      uniAddr = pairId pnproxy
      cid = chainId nproxy
      url = rpc nproxy
    Amount t1b <- getBalanceOf t1nproxy uniAddr
    Amount t2b <- getBalanceOf t2nproxy uniAddr
    let
      sellt1 (Amount t1) = Amount $ Q.calcInputPrice Q.defaultFee t1 t1b t2b
      buyt1 (Amount t2) = Amount $ Q.calcInputPrice Q.defaultFee t2 t2b t1b
      variance = undefined
    return $ ExchangeRate sellt1 buyt1 variance

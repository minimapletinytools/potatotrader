--{-# LANGUAGE AllowAmbiguousTypes   #-}
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

  OnChain(..),
  ChainCtx
) where

import           Control.Exception
import           Control.Monad.IO.Class
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
type ChainCtx n = (ExchangeCache (OnChain n), ExchangeAccount (OnChain n))
instance ExchangeCtx (OnChain n) ((),()) where
  cache = fst
  account = snd

class (Token t, Network n) => ChainToken t n where
  tokenAddress :: Proxy(t,n) -> Maybe Address
  tokenAddress _ = Nothing

instance ChainToken TT ThunderCoreMain

instance ChainToken USDT ThunderCoreMain where
  tokenAddress _ = Just "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"

type family BaseToken t where
  BaseToken TT = 'True
  BaseToken USDT = 'False

class (Token t1, Token t2, Network n, ExchangeCtx (OnChain n) c) => Uniswap (isbase :: Bool) t1 t2 n c where
  -- |
  -- exchanges t1 for t2 where input amount (t1) is fixed
  -- TODO remove chainid url and token arguments, no longer needed
  -- not sure why Proxy (isbase,n) doesn't work...
  t1t2SwapInput :: forall ex m. (MonadExchange (OnChain n) c m, Exception ex) => Proxy isbase -> Proxy (n,c) -> String -> Integer -> Address -> Amount t1 -> Amount t2 -> m (Either ex TxReceipt)

instance (Token t1, Token t2, Network n, ExchangeCtx (OnChain n) c) => Uniswap 'True t1 t2 n c where
  t1t2SwapInput _ _ url cid addr (Amount input_t1) (Amount min_t2) = liftIO . try $ Q.txEthToTokenSwapInput url cid addr (fromIntegral input_t1) (fromIntegral min_t2)

instance (Token t1, Token t2, Network n, ExchangeCtx (OnChain n) c) => Uniswap 'False t1 t2 n c where
  t1t2SwapInput _ _ url cid addr (Amount input_t1) (Amount min_t2) = liftIO . try $ Q.txTokenToEthSwapInput url cid addr (fromIntegral input_t1) (fromIntegral min_t2)

class (Token t1, Token t2, Network n) => UniswapNetwork t1 t2 n where
  uniswapAddress :: Proxy (t1,t2,n) -> Address

instance UniswapNetwork TT USDT ThunderCoreMain where
  uniswapAddress _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"

getBalanceOf :: forall t n c m. (MonadExchange (OnChain n) c m, ChainToken t n, Network n) => Proxy (t,n,c) -> Address -> m (Amount t)
getBalanceOf _ acct = let url = rpc (Proxy :: Proxy n) in
  case tokenAddress (Proxy :: Proxy (t,n)) of
    Just addr -> liftIO $ Amount <$> Q.getTokenBalanceOf url addr acct
    Nothing   ->  liftIO $ Amount <$> Q.getBalanceOf url acct

-- exchange
data OnChain n

instance (Network n) => Exchange (OnChain n) where
  exchangeName _ = networkName (Proxy :: Proxy n)
  type ExchangePairId (OnChain n) = Address
  type ExchangeCache (OnChain n) = ()
  type ExchangeAccount (OnChain n) = ()

-- Token Exchanges
data OnChainOrder = OnChainOrder {
  receipt :: TxReceipt
}

type ExchangeTokenConstraint t n c = (Token t, Exchange (OnChain n), ExchangeCtx (OnChain n) c, Network n, ChainToken t n)
instance ExchangeTokenConstraint t n c => ExchangeToken t (OnChain n) c where
  getBalance _ = let url = rpc (Proxy :: Proxy n) in
     case tokenAddress (Proxy :: Proxy (t,n)) of
       Just addr -> liftIO $ Amount <$> Q.getTokenBalance url addr
       Nothing   -> liftIO $ Amount <$> Q.getBalance url

type ExchangePairConstraint t1 t2 n c = (Network n, ExchangeTokenConstraint t1 n c, ExchangeTokenConstraint t2 n c, Uniswap (BaseToken t1) t1 t2 n c, Uniswap (BaseToken t2) t2 t1 n c, UniswapNetwork t1 t2 n)
instance ExchangePairConstraint t1 t2 n c => ExchangePair t1 t2 (OnChain n) c where
  pairId _ = uniswapAddress (Proxy :: Proxy(t1,t2,n))

  type Order t1 t2 (OnChain n) = OnChainOrder

  getStatus _ (OnChainOrder receipt) = do
    let url = rpc (Proxy :: Proxy n)
    v <- liftIO $ try (Q.getTransactionByHash url $ receiptTransactionHash receipt)
    case v of
      Left (SomeException _) -> return $ OrderStatus Missing
      Right _                -> return $ OrderStatus Executed

  -- TODO TEST

  order _ ot t1 t2 = do
    let
      nproxy = Proxy :: Proxy n
      ncproxy = Proxy :: Proxy (n,c)
      pproxy = Proxy :: Proxy (t1,t2,OnChain n,c)
      addr = pairId pproxy
      cid = chainId nproxy
      url = rpc nproxy
    v <- case ot of
      Buy  -> t1t2SwapInput (Proxy :: Proxy (BaseToken t1)) ncproxy url cid addr t1 t2
      Sell -> t1t2SwapInput (Proxy :: Proxy (BaseToken t2)) ncproxy url cid addr t2 t1
    case v of
      Left (SomeException _) -> undefined
      Right receipt          -> return $ OnChainOrder receipt

  -- TODO test
  getExchangeRate pnproxy = do
    let
      nproxy = Proxy :: Proxy n
      t1nproxy = Proxy :: Proxy (t1,n,c)
      t2nproxy = Proxy :: Proxy (t2,n,c)
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

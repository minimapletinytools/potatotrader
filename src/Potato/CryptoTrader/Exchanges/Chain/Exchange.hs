{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}


module Potato.CryptoTrader.Exchanges.Chain.Exchange (
  ThunderCoreMain(..),
  EthereumMain(..),

  OnChain(..),
  ChainCtx
) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Proxy
import           Data.Solidity.Prim.Address                (Address)
import           Network.Ethereum.Api.Types                (TxReceipt (..))
import qualified Potato.CryptoTrader.Exchanges.Chain.Query as Q
import           Potato.CryptoTrader.Types

uniswapTxFee :: Amount TT
uniswapTxFee = Amount (floor 4.7878e14)

-- | the network assosciated with a base token
class (Token n) => Network n where
  networkName :: Proxy n -> String
  rpc :: Proxy n -> String
  chainId :: Proxy n -> Integer
  -- | minimum balance in the base token suggested to pay for transaction fees
  minBalanceForGas :: Proxy n -> Amount n

type ThunderCoreMain = TT
type EthereumMain = ETH

instance Network TT where
  networkName _ = "ThunderCore mainnet"
  rpc _ = "https://mainnet-rpc.thundercore.com"
  chainId _ = 108
  minBalanceForGas _ = Amount $ 200000 * floor 1e10

instance Network ETH where
  networkName _ = "Ethereum mainnet"
  -- my private RPC url -__- don't use
  rpc _ = "https://mainnet.infura.io/v3/2edbdd953f714eeab3f0001bb0b96b91"
  chainId _ = 1
  minBalanceForGas _ = Amount $ 200000 * floor 1e10

-- helpers
type ChainCtx = ((),())
  -- = (ExchangeCache (OnChain n), ExchangeAccount (OnChain n))
  -- for all n

-- | data kind representing the type of uniswap operation
data SwapType = TokenToEth | EthToToken | TokenToToken

-- | type family for generating the swap type
type family GetSwapType t1 t2 n where
  GetSwapType t1 t2 t1 = 'TokenToEth
  GetSwapType t1 t2 t2 = 'EthToToken
  GetSwapType t1 t2 bt = 'TokenToToken

type family GetTokenType t n where
  GetTokenType t t = 'True
  GetTokenType t bt = 'False


-- | class for on chain tokens
-- includes both base tokens and ERC20 tokens
class (Token t, Network n) => ChainToken (isbase :: Bool) t n | t n -> isbase where
  tokenAddress :: Proxy(t,n) -> Maybe Address


-- | base tokens do not have ERC20 contract addresses
instance (Token bt, Network bt) => ChainToken 'True bt bt where
  tokenAddress _ = Nothing

instance ChainToken 'False USDT ThunderCoreMain where
  tokenAddress _ = Just "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"

-- TODO pretty sure I did the isbase thing wrong, i don't think it's necessary to specify the type when calling..... look it up again...
class (ChainToken (GetTokenType t1 n) t1 n, ChainToken (GetTokenType t2 n) t2 n) => Uniswap (stype :: SwapType) t1 t2 n | t1 t2 n -> stype where
  -- |
  -- exchanges t1 for t2 where input amount (t1) is fixed
  -- TODO remove chainid url and token arguments, no longer needed
  -- not sure why Proxy (isbase,n) doesn't work...
  -- TODO rename this to titjSwapInput since we frequently switch ti and tj around
  t1t2SwapInput :: forall ex m. (MonadExchange m, Exception ex) => Proxy n -> String -> Integer -> Address -> Amount t1 -> Amount t2 -> ExchangeT (OnChain n) m (Either ex TxReceipt)

instance (GetSwapType t1 t2 n~'EthToToken, ChainToken 'True t1 n, ChainToken 'False t2 n) => Uniswap 'EthToToken t1 t2 n where
  t1t2SwapInput _ url cid addr (Amount input_t1) (Amount min_t2) = liftIO . try $ do
    Q.txEthToTokenSwapInput url cid addr (fromIntegral input_t1) (fromIntegral min_t2)

instance (GetSwapType t1 t2 n~'TokenToEth, ChainToken 'False t1 n, ChainToken 'True t2 n) => Uniswap 'TokenToEth t1 t2 n where
  t1t2SwapInput _ url cid addr (Amount input_t1) (Amount min_t2) = liftIO . try $ do
    Q.txTokenToEthSwapInput url cid addr (fromIntegral input_t1) (fromIntegral min_t2)

class (ChainToken (GetTokenType t1 n) t1 n, ChainToken (GetTokenType t2 n) t2 n) => UniswapNetwork t1 t2 n where
  uniswapAddress :: Proxy (t1,t2,n) -> Address

instance UniswapNetwork TT USDT ThunderCoreMain where
  uniswapAddress _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"

getBalanceOf :: forall t n m. (MonadExchange m, ChainToken t n) => Proxy (t,n) -> Address -> ExchangeT (OnChain n) m (Amount t)
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

instance (ChainToken 'True t n) => ExchangeToken t (OnChain n) where
  getBalance _ = let url = rpc (Proxy :: Proxy n) in
     case tokenAddress (Proxy :: Proxy (t,n)) of
       Just _ -> throwM $ AssertionFailed "base token should not have contract address"
       Nothing   -> liftIO $ do
         b <- Amount <$> Q.getBalance url
         return $ max 0 (b - minBalanceForGas (Proxy :: Proxy n))

instance (ChainToken 'False t n) => ExchangeToken t (OnChain n) where
 getBalance _ = let url = rpc (Proxy :: Proxy n) in
    case tokenAddress (Proxy :: Proxy (t,n)) of
      Just addr -> liftIO $ Amount <$> Q.getTokenBalance url addr
      Nothing   -> throwM $ AssertionFailed "ERC20 should have contract address"

type ExchangePairConstraint t1 t2 n = (
  Uniswap (GetSwapType t1 t2 n) t1 t2 n,
  Uniswap (GetSwapType t2 t1 n) t2 t1 n,
  UniswapNetwork t1 t2 n)

instance ExchangePairConstraint t1 t2 n => ExchangePair t1 t2 (OnChain n) where
  pairId _ = uniswapAddress (Proxy :: Proxy(t1,t2,n))

  type Order t1 t2 (OnChain n) = OnChainOrder

  getStatus _ (OnChainOrder receipt) = do
    let url = rpc (Proxy :: Proxy n)
    v <- liftIO $ try (Q.getTransactionByHash url $ receiptTransactionHash receipt)
    case v of
      Left (SomeException _) -> return $ OrderStatus Missing
      Right _                -> return $ OrderStatus Executed

  -- TODO TEST

  order _ ofl ot t1 t2 = do
    let
      nproxy = Proxy :: Proxy n
      pproxy = Proxy :: Proxy (t1,t2,OnChain n)
      addr = pairId pproxy
      cid = chainId nproxy
      url = rpc nproxy
      flex :: forall t. Amount t -> Amount t
      flex = case ofl of
        Flexible -> Amount . floor . (*0.95) . fromIntegral
        Rigid    -> id
    v <- case ot of
      Buy  -> t1t2SwapInput nproxy url cid addr t2 (flex t1)
      Sell -> t1t2SwapInput nproxy url cid addr t1 (flex t2)
    case v of
      Left (SomeException e) -> do
        liftIO $ print e
        return undefined
      Right receipt          -> return $ OnChainOrder receipt

  -- TODO test
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
      -- TODO apply transaction fee, however, we need to know which one is the base token to do this
      -- make it a function of Uniswap class since it already knows which token is which type
      sellt1 (Amount t1) = Amount $ Q.calcInputPrice Q.defaultFee t1 t1b t2b
      buyt1 (Amount t2) = Amount $ Q.calcInputPrice Q.defaultFee t2 t2b t1b
      variance = undefined
    return $ ExchangeRate sellt1 buyt1 variance

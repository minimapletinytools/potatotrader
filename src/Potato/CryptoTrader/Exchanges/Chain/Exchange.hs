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


uniswapMaxGas :: Integer
uniswapMaxGas = 100000 -- about twice as much gas as needed

uniswapApproxGas :: Integer
uniswapApproxGas = 50000 -- still more than needed to be safe

-- network
-- TODO add a basetoken type parameter to this
-- or maybe simply change network to be the base token
class Network n where
  networkName :: Proxy n -> String
  rpc :: Proxy n -> String
  chainId :: Proxy n -> Integer
  -- | minimum balance in the base token suggested to pay for transaction fees
  -- TODO delete this, gas should be computed per operation using gasPrice instead
  minBalanceForGas :: Proxy n -> Integer
  minBalanceForGas p = gasPrice p * uniswapMaxGas
  gasPrice :: Proxy n -> Integer

data ThunderCoreMain
data EthereumMain

instance Network ThunderCoreMain where
  networkName _ = "ThunderCore mainnet"
  rpc _ = "https://mainnet-rpc.thundercore.com"
  chainId _ = 108
  gasPrice _ = floor 1e10

instance Network EthereumMain where
  networkName _ = "Ethereum mainnet"
  -- my private RPC url -__- don't use
  rpc _ = "https://mainnet.infura.io/v3/2edbdd953f714eeab3f0001bb0b96b91"
  chainId _ = 1
  gasPrice _ = floor 1e10

-- helpers
type ChainCtx = ((),())
  -- = (ExchangeCache (OnChain n), ExchangeAccount (OnChain n))
  -- for all n

class (Token t, Network n) => ChainToken t n where
  tokenAddress :: Proxy(t,n) -> Maybe Address
  tokenAddress _ = Nothing

instance ChainToken TT ThunderCoreMain

instance ChainToken USDT ThunderCoreMain where
  tokenAddress _ = Just "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"

type family BaseToken t where
  BaseToken TT = 'True
  BaseToken USDT = 'False

-- TODO pretty sure I did the isbase thing wrong, i don't think it's necessary to specify the type when calling..... look it up again...
class (ChainToken t1 n, ChainToken t2 n) => Uniswap (isbase :: Bool) t1 t2 n where
  -- |
  -- exchanges t1 for t2 where input amount (t1) is fixed
  -- TODO remove chainid url and token arguments, no longer needed
  -- not sure why Proxy (isbase,n) doesn't work...
  -- TODO rename this to titjSwapInput since we frequently switch ti and tj around
  t1t2SwapInput :: forall ex m. (MonadExchange m, Exception ex) => Proxy isbase -> Proxy n -> String -> Integer -> Address -> Amount t1 -> Amount t2 -> ExchangeT (OnChain n) m (Either ex TxReceipt)
  -- | this is uniswap's getInputPrice method with tx fee substracted away from either input or output base token
  t1t2InputPriceWithTxFee :: Proxy isbase -> Proxy n -> Amount t1 -> Amount t1 -> Amount t2 -> Amount t2

instance (ChainToken t1 n, ChainToken t2 n) => Uniswap 'True t1 t2 n where
  t1t2SwapInput _ _ url cid addr (Amount input_t1) (Amount min_t2) = liftIO . try $ do
    Q.txEthToTokenSwapInput url cid addr (fromIntegral input_t1) (fromIntegral min_t2)
  -- substract away fee from input amount
  t1t2InputPriceWithTxFee _ _ (Amount t1) (Amount t1b) (Amount t2b) = Amount (Q.calcInputPrice newt1 t1b t2b) where
    fee = uniswapApproxGas * gasPrice (Proxy :: Proxy n)
    newt1 = max 0 (t1-fee)

instance (ChainToken t1 n, ChainToken t2 n) => Uniswap 'False t1 t2 n where
  t1t2SwapInput _ _ url cid addr (Amount input_t1) (Amount min_t2) = liftIO . try $ do
    Q.txTokenToEthSwapInput url cid addr (fromIntegral input_t1) (fromIntegral min_t2)
  -- substract away fee from output amount
  t1t2InputPriceWithTxFee _ _ (Amount t1) (Amount t1b) (Amount t2b) = Amount $ max 0 (Q.calcInputPrice t1 t1b t2b - fee) where
    fee = uniswapApproxGas * gasPrice (Proxy :: Proxy n)

class (ChainToken t1 n, ChainToken t2 n) => UniswapNetwork t1 t2 n where
  uniswapAddress :: Proxy (t1,t2,n) -> Address

-- this seems a little unfortunate to me...
instance UniswapNetwork TT USDT ThunderCoreMain where
  uniswapAddress _ = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"
instance UniswapNetwork USDT TT ThunderCoreMain where
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
data OnChainOrder t1 t2 = OnChainOrder {
  receipt                   :: TxReceipt
  , chainOrderType          :: OrderType
  -- inputs to the swap function basically
  -- note that the actualy min output amount is reduced in the `Flexible` order type case
  , chainOrderDesiredAmount :: (Amount t1, Amount t2)
}

instance ChainToken t n => ExchangeToken t (OnChain n) where
  getBalance _ = let url = rpc (Proxy :: Proxy n) in
     case tokenAddress (Proxy :: Proxy (t,n)) of
       Just addr -> liftIO $ Amount <$> Q.getTokenBalance url addr
       Nothing   -> liftIO $ do
         b <- Q.getBalance url
         return . Amount $ max 0 (b - minBalanceForGas (Proxy :: Proxy n))

type ExchangePairConstraint t1 t2 n = (
  Uniswap (BaseToken t1) t1 t2 n,
  Uniswap (BaseToken t2) t2 t1 n,
  UniswapNetwork t1 t2 n)

instance ExchangePairConstraint t1 t2 n => ExchangePair t1 t2 (OnChain n) where
  pairId _ = uniswapAddress (Proxy :: Proxy(t1,t2,n))

  type Order t1 t2 (OnChain n) = OnChainOrder t1 t2

  getStatus _ (OnChainOrder receipt ot amt) = do
    let url = rpc (Proxy :: Proxy n)
    v <- liftIO $ try (Q.getTransactionByHash url $ receiptTransactionHash receipt)
    case v of
      Left (SomeException _) -> return $ defOrderStatus
      -- TODO THIS IS INCORRECT
      -- the correct way to do this is to store the transaction parameters in OnChainOrder
      -- and then make a web3 call using the same input parameters at the block number in the recipt to obtain the output
      -- you can do this using Network.Ethereum.Api.Eth.call method
      Right _                -> return $ OrderStatus Executed ot amt

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
      Buy  -> t1t2SwapInput (Proxy :: Proxy (BaseToken t2)) nproxy url cid addr t2 (flex t1)
      Sell -> t1t2SwapInput (Proxy :: Proxy (BaseToken t1)) nproxy url cid addr t1 (flex t2)
    case v of
      Left (SomeException e) -> do
        liftIO $ print e
        return undefined
      Right receipt          -> return $ OnChainOrder receipt ot (t1,t2)

  -- TODO test
  getExchangeRate pnproxy = do
    let
      nproxy = Proxy :: Proxy n
      t1nproxy = Proxy :: Proxy (t1,n)
      t2nproxy = Proxy :: Proxy (t2,n)
      uniAddr = pairId pnproxy
      cid = chainId nproxy
      url = rpc nproxy
    t1b <- getBalanceOf t1nproxy uniAddr
    t2b <- getBalanceOf t2nproxy uniAddr
    let
      -- TODO apply transaction fee, however, we need to know which one is the base token to do this
      -- make it a function of Uniswap class since it already knows which token is which type
      sellt1 t1 = t1t2InputPriceWithTxFee (Proxy :: Proxy (BaseToken t1)) nproxy t1 t1b t2b
      buyt1 t2 = t1t2InputPriceWithTxFee (Proxy :: Proxy (BaseToken t2)) nproxy t2 t2b t1b
      variance = undefined
    return $ ExchangeRate sellt1 buyt1 variance

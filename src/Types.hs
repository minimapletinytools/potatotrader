{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE TypeFamilies               #-}


module Types (
  Amount(..),
  Liquidity(..),
  OrderType(..),
  ExchangeRate(..),

  Token(..),
  fromStdDenom,
  ExchangeCtx,
  MonadExchange,
  Exchange(..),
  ExchangeToken(..),
  ExchangePair(..),
  OrderState(..),
  OrderStatus(..),
  Order,

  TT(..),
  ETH(..),
  USDT(..),
  SAI(..)
) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Proxy
import           Data.Solidity.Prim.Address (Address)

-- | type safe representation of a currency amount in its base (smallest) denomination
newtype Amount t = Amount Integer deriving (Eq, Ord, Num, Show, Read, Enum, Real)

data Liquidity t1 t2 = Liquidity (Amount t1) (Amount t2)
data OrderType = Buy | Sell deriving (Eq, Show)

-- TODO maybe make this into a type class so that you can have something like buyt2 = sellt1
-- TODO these methods do not consider the case where there is not enough market to complete the order thus not all of input is spent for output
data ExchangeRate t1 t2 = ExchangeRate {
  -- | sellt1 returns approx amount of t2 bought for input of t1
  sellt1     :: Amount t1 -> Amount t2
  -- | buyt1 returns approx amount of t1 bought for input of t2
  , buyt1    :: Amount t2 -> Amount t1
  -- | variance returs the variance of the quantity |desired_t1/desired_t2 - actual_t1/actual_t2|
  -- does not distinguish between buy/sell
  -- TODO this should probably return something like (TimeDiff -> Double)
  , variance :: Amount t1 -> Amount t2 -> Double
}

instance (Token t1, Token t2) => Show (ExchangeRate t1 t2) where
  show (ExchangeRate sellt1' buyt1' variance') = output where
    amounts = map (10^) [0..5]
    unAmount (Amount x) = x
    sellChart = map (unAmount . sellt1' . Amount) amounts
    buyChart = map (unAmount . buyt1' . Amount) amounts
    output = "sell: " ++ unwords (zipWith (\a b -> show a ++":"++ show b) amounts sellChart) ++ "\n"
      ++ "buy: " ++ unwords (zipWith (\a b -> show a ++":"++ show b) amounts buyChart) ++ "\n"

class Token t where
  tokenName :: Proxy t -> String
  decimals :: Proxy t -> Integer

-- | fromStdDenom converts currency in standard demonation to base demonation
fromStdDenom :: forall t. (Token t) => Integer -> Amount t
fromStdDenom x = Amount (x * decimals (Proxy :: Proxy t))

class Exchange e where
  exchangeName :: Proxy e -> String
  type ExchangePairId e :: *
  -- TODO something like this? However, we either need to use mutable cache (doable since everything we need it for is IO) or have all types return the cache as well
  type ExchangeCache e :: *
  -- TODO generalize account access to the exchange
  type ExchangeAccount e :: *

type ExchangeCtx e = (ExchangeCache e, ExchangeAccount e)
type MonadExchange e m = (MonadThrow m, MonadIO m, MonadReader (ExchangeCtx e) m)

class (Token t, Exchange e) => ExchangeToken t e where
  -- TODO probably don't need this, it's encapsulated by getBalance
  -- symbol of token on the exchange
  symbol :: Proxy (t,e) -> String
  symbol _ = tokenName (Proxy :: Proxy t)
  -- get balance (normalized to lowest denomination)
  getBalance :: (MonadExchange e m) => Proxy (t,e) -> m (Amount t)


data OrderState = Pending | PartiallyExecuted | Executed | Cancelled | Missing deriving (Show)
data OrderStatus = OrderStatus {
  orderState :: OrderState
}

-- maybe simpler way to do type level exchange pairs
class (ExchangeToken t1 e, ExchangeToken t2 e) => ExchangePair t1 t2 e where
  pairName :: Proxy (t1,t2,e) -> String
  pairName _ =
    exchangeName (Proxy :: Proxy e) ++ " "
    ++ tokenName (Proxy :: Proxy t1) ++ ":"
    ++ tokenName (Proxy :: Proxy t2)

  -- | pairID returns a String identifier
  pairId :: Proxy (t1,t2,e) -> ExchangePairId e

  -- | liquidity returns your respective balance in the two tokens
  -- TODO is this the right name for it?
  -- TODO probably just delete this function, there's no reason an exchange would override this implementation
  liquidity :: (MonadExchange e m) => Proxy (t1,t2,e) -> m (Liquidity t1 t2)
  liquidity _ = do
    b1 <- getBalance (Proxy :: Proxy (t1, e))
    b2 <- getBalance (Proxy :: Proxy (t2, e))
    return $ Liquidity b1 b2

  -- | getExchangeRate returns the current exchange rate
  getExchangeRate :: (MonadExchange e m) => Proxy (t1,t2,e) -> m (ExchangeRate t1 t2)

  type Order t1 t2 e :: *
  -- | getOrders returns all unexecuted orders
  getOrders :: (MonadExchange e m) => Proxy (t1,t2,e) -> m [Order t1 t2 e]
  getOrders _ = return []
  -- | order buys t1 for t2 tokens OR sells t1 for t2 tokens
  order :: (MonadExchange e m) => Proxy (t1,t2,e) -> OrderType -> Amount t1 -> Amount t2 -> m (Order t1 t2 e)
  getStatus :: (MonadExchange e m) => Proxy (t1,t2,e) -> Order t1 t2 e -> m OrderStatus
  -- TODO make this a parameter of the exchange, not the exchange pair?
  canCancel :: Proxy (t1,t2,e) -> Order t1 t2 e -> Bool -- or is this a method of OrderStatus?
  canCancel _ _ = False
  cancel :: (MonadExchange e m) => Proxy (t1,t2,e) -> Order t1 t2 e -> m Bool
  cancel = undefined







-- TODO maybe move to a diff file
-- tokens
data TT
data ETH
data USDT
data SAI

instance Token TT where
  tokenName _ = "TT"
  decimals _ = 1e18

instance Token ETH where
  tokenName _ = "ETH"
  decimals _ = 1e18

instance Token USDT where
  tokenName _ = "USDT"
  decimals _ = 1e6

instance Token SAI where
  tokenName _ = "SAI"
  decimals _ = 1e18




-- below is a WIP, lots of typing issues go away
{-
class (Token t1, Token t2) => ExchangePair t1 t2 e where
  getLiquidity :: e -> IO (Liquidity t1 t2)

data TokenExchange t1 t2 where
  TokenExchange :: (ExchangeToken t1 e1, ExchangeToken t2 e2) => TokenExchange t1 t2

instance (ExchangeToken t1 e1, ExchangeToken t2 e2) => ExchangePair t1 t2 (TokenExchange t1 t2) where
  getLiquidity :: TokenExchange t1 t2 -> IO (Liquidity t1 t2)
  getLiquidity _ = do
    b1 <- getBalance (Proxy :: Proxy (t1, e1))
    b2 <- getBalance (Proxy :: Proxy (t2, e2))
    return $ Liquidity b1 b2


data ExchangeExchange t1 t2 t3 where
  ExchangeExchange :: (ExchangePair t1 t2 e12, ExchangePair t2 t3 e23) => e12 -> e23 -> ExchangeExchange t1 t2 t3

-- broken due to GADTs not carrying scope on their type variables
-- (so the t1 t2 t3 in "data ExchangeExchange t1 t2 t3" do not match the t1 t2 t3 in th ctor)
instance (Token t1, Token t2, Token t3) => ExchangePair t1 t3 (ExchangeExchange t1 t2 t3) where
  -- this is hard because need to query exchange rate to convert t2 balance into liquidity
  -- for now, ignore t2 balance
  getLiquidity :: ExchangeExchange t1 t2 t3 -> IO (Liquidity t1 t3)
  getLiquidity (ExchangeExchange e12 e23) = do
    Liquidity l1 _ <- getLiquidity e12
    Liquidity _ l3 <- getLiquidity e23
    return $ Liquidity l1 l3
-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE TypeFamilies               #-}


module Potato.Trader.Types (

  -- TODO move Amount stuff into its own module
  Amount(..),
  applyFee,
  AmountRatio(..),
  makeRatio,
  ($:$*),
  (*$:$),
  (/$:$),
  ($:$*$:$),
  ($:$/$:$),

  Liquidity(..),
  OrderType(..),
  OrderFlex(..),
  ExchangeRate(..),

  Token(..),
  toStdDenom,
  fromStdDenom,
  ratioToStdDenom,
  stdDenomToRatio,
  ExchangeCtx,
  MonadExchange,
  ExchangeT,
  Exchange(..),
  ExchangeToken(..),
  ExchangePair(..),
  OrderState(..),
  OrderStatus(..),
  defOrderStatus,
  orderByPrice,

  -- TODO move these to a different file
  TT,
  ETH,
  USDT,
  SAI
) where

import           Control.DeepSeq        (NFData)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Proxy
import           GHC.Generics

-- | type safe representation of a currency amount in its base (smallest) denomination
newtype Amount t = Amount Integer
  deriving (Eq, Ord, Read, Enum, Num, Integral, Real, Generic, NFData)

instance (Token t) => Show (Amount t) where
  show t = show (toStdDenom t) ++ " " ++ tokenName (Proxy :: Proxy t)

applyFee :: AmountRatio t t -> Amount t -> Amount t
applyFee (AmountRatio f) = Amount . floor . ((1-f) *) . fromIntegral

-- | type safe representation of a currency exchange ratio t1:t2 in its base (smallest) denomination
newtype AmountRatio t1 t2 = AmountRatio Double
  deriving (Eq, Ord, Show, Read, Enum, Num, Real, Fractional, RealFrac, Generic, NFData)

makeRatio :: Amount t1 -> Amount t2 -> AmountRatio t1 t2
makeRatio t1 t2 = fromIntegral t1 / fromIntegral t2

($:$*) :: AmountRatio t1 t2 -> Amount t2 -> Amount t1
($:$*) t1_over_t2 t1 = Amount $ floor . (t1_over_t2 *) . fromIntegral $ t1
infixl 7 $:$*

(*$:$) :: Amount t2 -> AmountRatio t1 t2 -> Amount t1
(*$:$) = flip ($:$*)
infixl 7 *$:$

(/$:$) :: Amount t1 -> AmountRatio t1 t2 -> Amount t2
(/$:$) t1 t1_over_t2 = Amount $ floor $ fromIntegral t1 / t1_over_t2
infixl 7 /$:$

($:$*$:$) :: AmountRatio t1 t2 -> AmountRatio t2 t1 -> Double
($:$*$:$)  (AmountRatio r1) (AmountRatio r2) = r1*r2
infixl 7 $:$*$:$

($:$/$:$) :: AmountRatio t1 t2 -> AmountRatio t1 t2 -> Double
($:$/$:$)  (AmountRatio r1) (AmountRatio r2) = r1/r2
infixl 7 $:$/$:$

data Liquidity t1 t2 = Liquidity (Amount t1) (Amount t2)

-- | the type of order
-- for a token pair `t1,t2`, `Buy` and `Sell` refers to buying and selling `t1` respectively
data OrderType = Buy | Sell deriving (Eq, Show)

-- TODO add a parameter to Flexible orders on how much the price is allowed to flex :)
-- | flexibility of an order
-- `Rigid` means the order must meet the set price or be better
-- `Flexible` gives flexibility to the order price
data OrderFlex = Flexible | Rigid deriving (Eq, Show)

-- | Data type that abstracts exchange rates as functions.
-- The interface is likely to be upgraded in the future as thu current design is limited.
data ExchangeRate t1 t2 = ExchangeRate {
  -- | sellt1 returns approx amount of t2 bought for input of t1 including fees
  sellt1     :: Amount t1 -> Amount t2
  -- | buyt1 returns approx amount of t1 bought for input of t2 including fees
  , buyt1    :: Amount t2 -> Amount t1
  -- | variance returs the variance of the quantity |desired_t1/desired_t2 - actual_t1/actual_t2|
  -- does not distinguish between buy/sell
  -- TODO this should probably return something like (TimeDiff -> Double)
  , variance :: Amount t1 -> Amount t2 -> Double

  -- TODO
  --feet1 :: Amount2 -> Amount t1 -> Amount t1
  --feet2 :: Amount1 -> Amount t2 -> Amount t2
}

-- TODO consider making ExchangeRate into a type class to de the below two functions more elegantly
--buyt2 :: ExchangeRate t1 t2 -> (Amount t1 -> Amount t2)
--buyt2 = sellt1
--sellt2 :: ExchangeRate t1 t2 -> (Amount t2 -> Amount t1)
--sellt2 = buyt1

-- | A class for tradeable tokens
class Token t where
  tokenName :: Proxy t -> String
  decimals :: Proxy t -> Integer

-- TODO write invert tests for these
-- | converts currency from base denomination to standard denomination
toStdDenom :: forall t. (Token t) => Amount t -> Double
toStdDenom (Amount t) = fromIntegral t / fromIntegral (decimals (Proxy :: Proxy t))

-- | converts currency from standard demonation to base demonation
fromStdDenom :: forall t. (Token t) => Double -> Amount t
fromStdDenom = Amount . floor . (* fromInteger (decimals (Proxy :: Proxy t)))

-- | converts ratio from base denomination to standard denomination
ratioToStdDenom :: forall t1 t2. (Token t1, Token t2) => AmountRatio t1 t2 -> Double
ratioToStdDenom (AmountRatio r) = r * fromIntegral (decimals (Proxy :: Proxy t2)) / fromIntegral (decimals (Proxy :: Proxy t1))

-- | converts ratio from standard demonation to base demonation
stdDenomToRatio :: forall t1 t2. (Token t1, Token t2) => Double -> AmountRatio t1 t2
stdDenomToRatio r = AmountRatio $ r * fromIntegral (decimals (Proxy :: Proxy t1)) / fromIntegral (decimals (Proxy :: Proxy t2))

-- | not an especially pretty implementation, just for debugging purposes
instance (Token t1, Token t2) => Show (ExchangeRate t1 t2) where
  show (ExchangeRate sellt1' buyt1' _) = output where
    amounts = map (10^) [0..5 :: Integer]
    unAmount (Amount x) = x
    sellChart = map (unAmount . sellt1' . Amount) amounts
    buyChart = map (unAmount . buyt1' . Amount) amounts
    output = "sell: " ++ unwords (zipWith (\a b -> show a ++":"++ show b) amounts sellChart) ++ "\n"
      ++ "buy: " ++ unwords (zipWith (\a b -> show a ++":"++ show b) amounts buyChart) ++ "\n"

-- | A class for exchanges
class Exchange e where
  -- | the name of the exchange
  exchangeName :: Proxy e -> String
  -- | type family for the exchange (trading) pair id
  -- For example, for a on chain uniswap exchange, the pair id is the uniswap contract address
  type ExchangePairId e :: *
  -- TODO rename to ExchangeData, as we want it to store parameters and cache and whatever else makes sense to the user
  -- | intended to store mutable references for stuff like caches and connection analytics or whatever
  type ExchangeData e :: *
  -- TODO generalize account access to the exchange
  -- | private account access to exchange API
  type ExchangeAccount e :: *

type ExchangeCtx e = (ExchangeData e, ExchangeAccount e)

-- | this constraint kind is for the monad in which all our exchange operations take place in
type MonadExchange m = (Monad m, MonadCatch m, MonadIO m)

-- | the concrete monad transformer that is used by all our exchange operations
-- must satisfy type constraint (MonadExchange m)
--type ExchangeT e m = forall e m . (MonadExchange m) => ReaderT (ExchangeCtx e) m
type ExchangeT e m = ReaderT (ExchangeCtx e) m

-- TODO rename this
-- | A class for tokens in an exchange
-- Account refers to the account stored in `ExchangeAccount e` of the `c` context parameter.
class (Token t, Exchange e) => ExchangeToken t e where
  -- TODO probably don't need this, it's encapsulated by getBalance
  -- symbol of token on the exchange
  symbol :: Proxy (t,e) -> String
  symbol _ = tokenName (Proxy :: Proxy t)
  -- get the account's balance (normalized to lowest denomination)
  getBalance :: (MonadExchange m) => Proxy (t,e) -> ExchangeT e m (Amount t)

-- | the state of an order
data OrderState = Pending | PartiallyExecuted | Executed | Cancelled | Missing deriving (Eq, Show)

-- | the status of an order
data OrderStatus t1 t2 = OrderStatus {
  orderState        :: OrderState
  , orderType       :: OrderType -- buy or sell t1 for t2
  , orderOrigAmount :: (Amount t1, Amount t2)
  , orderExecAmount :: (Amount t1, Amount t2)
} deriving (Show)

defOrderStatus :: OrderStatus t1 t2
defOrderStatus = OrderStatus Missing Buy (0,0) (0,0)

-- | A class for tradeable token pairs on an exchange.
-- All opreations of `ExchangePair t1 t2` are from the perspective of `t1`
-- i.e. buying and selling refers to what we are doing with `t1`
-- Account refers to the account stored in `ExchangeAccount e` of the `c` context parameter.
class (ExchangeToken t1 e, ExchangeToken t2 e) => ExchangePair t1 t2 e where
  -- | The name of the exchange pair
  pairName :: Proxy (t1,t2,e) -> String
  pairName _ =
    exchangeName (Proxy :: Proxy e) ++ " "
    ++ tokenName (Proxy :: Proxy t1) ++ ":"
    ++ tokenName (Proxy :: Proxy t2)

  -- | The exchange's identifier for this trading pair
  pairId :: Proxy (t1,t2,e) -> ExchangePairId e

  -- TODO is this the right name for it?
  -- TODO probably just delete this function, there's no reason an exchange would override this implementation
  -- | returns the account's respective balance for both tokens
  liquidity :: (MonadExchange m) => Proxy (t1,t2,e) -> ExchangeT e m (Liquidity t1 t2)
  liquidity _ = do
    b1 <- getBalance (Proxy :: Proxy (t1,e))
    b2 <- getBalance (Proxy :: Proxy (t2,e))
    return $ Liquidity b1 b2

  -- | returns the current exchange rate
  getExchangeRate :: (MonadExchange m) => Proxy (t1,t2,e) -> ExchangeT e m (ExchangeRate t1 t2)

  -- | type family for the exchange's order type
  type Order t1 t2 e :: *

  -- | returns all unexecuted orders
  getOrders :: (MonadExchange m) => Proxy (t1,t2,e) -> ExchangeT e m [Order t1 t2 e]
  getOrders _ = return []

  -- | buys `t1` for `t2` tokens OR sells `t1` for `t2` tokens
  order :: (MonadExchange m) => Proxy (t1,t2,e) -> OrderFlex -> OrderType -> Amount t1 -> Amount t2 -> ExchangeT e m (Order t1 t2 e)

  -- | returns the status of an order
  getStatus :: (MonadExchange m) => Proxy (t1,t2,e) -> Order t1 t2 e -> ExchangeT e m (OrderStatus t1 t2)

  -- TODO DELETE
  -- | delete this because cancel will just return false if it can't be cancelled..
  canCancel :: Proxy (t1,t2,e) -> Order t1 t2 e -> Bool -- or is this a method of OrderStatus?
  canCancel _ _ = False

  -- | cancels an order
  -- returns whether the order was cancelled successfully or not
  cancel :: (MonadExchange m) => Proxy (t1,t2,e) -> Order t1 t2 e -> ExchangeT e m Bool
  cancel _ _ = return False

orderByPrice ::
  (ExchangePair t1 t2 e, MonadExchange m)
  => Proxy (t1,t2,e)
  -> OrderFlex
  -> OrderType
  -> AmountRatio t2 t1 -- ^ price in t2
  -> Amount t1 -- ^ volume in t1
  -> ExchangeT e m (Order t1 t2 e)
orderByPrice pproxy ofl ot price volume = order pproxy ofl ot at1 at2 where
  at1 = volume
  at2 = price $:$* volume





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

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}


module Arbitrage (
  CtxPair(..),
  ExchangePairT(..),
  ArbitrageLogs,
  ArbitrageConstraints,
  doArbitrage
) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import           Data.Semigroup
import           Types


type CtxSingle e = (ExchangeCache e, ExchangeAccount e)

-- | context tuple for operations on two exchanges in the same monad
-- newtype wrapper needed to avoid duplicate instances
type CtxPair e1 e2 = (CtxSingle e1, CtxSingle e2)

-- | constraint kind needed for arbitrage operations
type ArbitrageConstraints t1 t2 e1 e2 m = (
  ExchangePair t1 t2 e1
  , ExchangePair t1 t2 e2
  , MonadExchange m
  )

-- | logging type for arbitrage
data ArbitrageLogs = ArbitrageLogs deriving (Show)

-- TODO
instance Semigroup ArbitrageLogs where
  (<>) = const

instance Monoid ArbitrageLogs where
  mempty = ArbitrageLogs

-- | monad type used for arbitrage which allows operating on two exchanges at the same time
type ExchangePairT e1 e2 m = ReaderT (CtxPair e1 e2) m

-- TODO figure out type signature
-- using this type signature creates ambiguous type var error.. don't entirely understand why because CtxSingle e1/e2 should always tuples inside of CtxPair e1 e2
--lifte1 :: forall e1 e2 m a. (Exchange e1, Exchange e2) => ExchangeT e1 m a -> ExchangePairT e1 e2 m a

-- | lift a reader action on r to a reader action on (r,b)
-- this matches the type `ExchangeT e1 m a -> ExchangePairT e1 e2 m a`
lifte1 :: ReaderT r m a -> ReaderT (r, b) m a
lifte1 a = ReaderT $ \(c1,c2) -> runReaderT a c1

-- | lift a reader action on r to a reader action on (b,r)
-- this matches the type `ExchangeT e2 m a -> ExchangePairT e1 e2 m a`
lifte2 :: ReaderT r m a -> ReaderT (b, r) m a
lifte2 a = ReaderT $ \(c1,c2) -> runReaderT a c2

doArbitrage :: forall t1 t2 e1 e2 m. (ArbitrageConstraints t1 t2 e1 e2 m, MonadWriter ArbitrageLogs (ExchangePairT e1 e2 m)) =>
  Proxy (t1, t2, e1, e2)
  -> ExchangePairT e1 e2 m ()
doArbitrage proxy = do

  -- query and cancel all orders
  qncresult <- try $ do
    let
      pe1 = Proxy :: Proxy (t1,t2,e1)
      pe2 = Proxy :: Proxy (t1,t2,e2)
    --(c1,c2) <- ask
    --e1orders <- ReaderT $ const $ flip runReaderT c1 $ getOrders pe1
    lifte1 $ do
      e1orders <- getOrders pe1
      mapM_ (cancel pe1) e1orders
    lifte2 $ do
      e2orders <- getOrders pe2
      mapM_ (cancel pe2) e2orders
  case qncresult of
    -- TODO log and error and restart
    Left (SomeException e) -> return ()
    Right _                -> return ()

  -- query balances
  gbresult <- try $ do
    t1e1 <- lifte1 $ getBalance (Proxy :: Proxy (t1, e1))
    t2e1 <- lifte1 $ getBalance (Proxy :: Proxy (t2, e1))
    t1e2 <- lifte2 $ getBalance (Proxy :: Proxy (t1, e2))
    t2e2 <- lifte2 $ getBalance (Proxy :: Proxy (t2, e2))
    return (t1e1, t2e1, t1e2, t2e2)
  (t1e1, t2e1, t1e2, t2e2) <- case gbresult of
    -- TODO log and error and restart
    Left (SomeException e) -> return (0,0,0,0)
    Right r                -> return r

  -- query exchange rate
  erresult <- try $ do
    exchRate1 <- lifte1 $ getExchangeRate (Proxy :: Proxy (t1,t2,e1))
    exchRate2 <- lifte2 $ getExchangeRate (Proxy :: Proxy (t1,t2,e2))
    return (exchRate1, exchRate2)
  (exchRate1, exchRate2) <- case erresult of
    -- TODO log and error and restart
    Left (SomeException e) -> undefined
    Right r                -> return r

  {-
  -- TODO something like this
  --if t1e1/t1e2 > t2e1/t2e2 then profit_t1e2 else profit_t2e1

  -- TODO abstract this in t1 t2 e1 e2 and use in commented line of code above
  -- TODO add tx fees...
  let
    sellt1_e1 = sellt1 exchRate1
    buyt1_e2 = buyt1 exchRate2

    --t1:t2 exchange ratio for input amount t1e1_in on exchange e1
    --in this case, we are selling t1 on e1
    t1t2e1 t1e1_in = t1e1_in / sellt1_e1 t1e1_in

    --t2:t1 exchange ratio for input amount t1e2_in on exchange e2
    -- in this case, we are buying t1 on e2
    t2t1e2 t2e2_in = buyt1_e2 t2e2_in / t2e2_in

    -- profit of t1 on exchange e2 after successful arbitrage
    profit_t1e2 t1e1_in = 1/(t1t2e1 t1e1_in * t2t1e2 (sellt1_e1 t1e1_in))

    -- TODO maximize profit heuristically

    -- TODO execute the trades if profit exceeds threshold
    -- add all trades to a list

  -- TODO sleep based on rate limit for api calls
  -}

  return ()

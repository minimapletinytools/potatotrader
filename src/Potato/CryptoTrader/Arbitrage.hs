{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}


module Potato.CryptoTrader.Arbitrage (
  CtxPair(..),
  ExchangePairT(..),
  ArbitrageLogs,
  ArbitrageConstraints,
  doArbitrage,

  -- exported for testing
  searchMax
) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Control.Parallel.Strategies
import           Data.Proxy
import           Data.Semigroup
import           Potato.CryptoTrader.Types

import           Debug.Trace


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


-- TODO move to a different file
-- | cancels all unexecuted or partially executed orders
cancelAllOrders :: (ExchangePair t1 t2 e, MonadExchange m) => Proxy (t1, t2, e) -> ExchangeT e m ()
cancelAllOrders p = do
  orders <- getOrders p
  mapM_ (cancel p) orders

-- | check for arbitrage opportunities and submits orders if profitable
-- returns orders submitted
--
-- arbitrage terminology
-- * b_tiek - balance in ti tokens on ek
-- * titjek in_tiek - exchange rate ti:tj on ek as a function of ti input tokens on ek
--  e.g. t1t2e1 in_t1e1 - exchange rate of t1:t2 on e1 as a function of t1 input tokens on e1
--  N.B that t1t2ek and t2t1ek are usually different
-- * profit_tiek - profit in ti tokens on exchange ek (after arbitrage with el and ek)
--  e.g. profit_t1e2 - profit in t1 tokens on exchange e2
--
doArbitrage :: forall t1 t2 e1 e2 m. (ArbitrageConstraints t1 t2 e1 e2 m, MonadWriter ArbitrageLogs (ExchangePairT e1 e2 m)) =>
  Proxy (t1, t2, e1, e2)
  -> ExchangePairT e1 e2 m ()
doArbitrage _ = do

  -- query and cancel all orders
  qncresult <- try $ do
    let
      pe1 = Proxy :: Proxy (t1,t2,e1)
      pe2 = Proxy :: Proxy (t1,t2,e2)
    lifte1 (cancelAllOrders pe1)
    lifte2 (cancelAllOrders pe2)
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
  (b_t1e1, b_t2e1, b_t1e2, b_t2e2) <- case gbresult of
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

  let
    sellt1_e1 = sellt1 exchRate1
    buyt1_e1 = buyt1 exchRate1
    sellt1_e2 = sellt1 exchRate2
    buyt1_e2 = buyt1 exchRate2

  case profit_t1 (Proxy :: Proxy (t1,t2,e1,e2)) (b_t1e1, b_t2e1) (b_t1e2, b_t2e2) sellt1_e1 buyt1_e1 sellt1_e2 buyt1_e2 of
    Left in_t1e1 -> do
      let
        out_t2e1 = sellt1_e1 in_t1e1
        in_t2e2 = out_t2e1
        out_t1e2 = buyt1_e2 in_t2e2
      -- sell t1 on e1
      --lifte1 $ order (Proxy :: Proxy (t1,t2,e1)) Sell in_t1e1 out_t2e1
      -- buy t1 on e2
      --lifte2 $ order (Proxy :: Proxy (t1,t2,e2)) Buy out_t1e2 in_t2e2
      trace (show (in_t1e1, out_t2e1, out_t1e2)) $ return ()
    Right in_t1e2 ->
      trace "other way" $ undefined

  return ()




-- |
profit_t1 ::
  forall t1 t2 e1 e2. (Token t1, Token t2, Exchange e1, Exchange e2)
  => Proxy (t1,t2,e1,e2) -- ^ proxy to help make "type bindings" in function name explicit. Note that there is no need fo constraints on e1 and e2.
  -> (Amount t1, Amount t2) -- ^ e1 balances
  -> (Amount t1, Amount t2) -- ^ e2 balances
  -> (Amount t1 -> Amount t2) -- ^ sellt1_e1
  -> (Amount t2 -> Amount t1) -- ^ buyt1_e1
  -> (Amount t1 -> Amount t2) -- ^ sellt1_e2
  -> (Amount t2 -> Amount t1) -- ^ buyt1_e2
  -> Either (Amount t1) (Amount t1) -- ^ amount of t1 or t2 to arbitrage (Left means on e1 and Right means on e2)
profit_t1 _ (b_t1e1, b_t2e1) (b_t1e2, b_t2e2) sellt1_e1 buyt1_e1 sellt1_e2 buyt1_e2 = r where

    -- construct profit functions
    profit_t1e1 = profit_tiek (Proxy :: Proxy (t1,t2,e1,e2)) sellt1_e2 buyt1_e1 b_t2e1
    profit_t1e2 = profit_tiek (Proxy :: Proxy (t1,t2,e2,e1)) sellt1_e1 buyt1_e2 b_t2e2

    -- always profit on t1 for now
    res = [50,10,10,10]
    pt1e1 = searchMax res (0,b_t1e2) profit_t1e1
    pt1e2 = searchMax res (0,b_t1e1) profit_t1e2
    r = if pt1e1 > pt1e2 then Left pt1e1 else Right pt1e2

    -- TODO figure out conditions for profitting on t2 instead of t1 (to maximize arbitrage potential before more liquidity is needed in one exchange or the other)
    --profit_t2e2 = profit_tiek (Proxy :: Proxy (t2,t1,e2,e1)) buyt1_e1 sellt1_e2
    --profit_t2e1 = profit_tiek (Proxy :: Proxy (t2,t1,e1,e2)) buyt1_e2 sellt1_e1
    --this in incorrect, it's exchange specific
    --fi = fromIntegral
    --do_t1 = fi b_t1e1 / fi b_t1e2 > fi b_t2e1 / fi b_t2e2
    --arbitrage to profit in t2
    --pt2e1 = searchMax res (0,b_t2e2) profit_t2e1
    --pt2e2 = searchMax res (0,b_t2e1) profit_t2e2
    --if pt2e1 > pt2e2 then Left pt2e1 else Right pt2e2

-- |
-- profit in ti tokens on exchange ek (after arbitrage ti->tj on el and tj->ti on ek)
profit_tiek ::
  (Token ti, Token tj, Exchange ek, Exchange el)
  => Proxy (ti, tj, el, ek) -- ^ proxy to help make "type bindings" in function name explicit. Note that there is no need fo constraints on e1 and e2.
  -> (Amount ti -> Amount tj) -- ^ sellti_el
  -> (Amount tj -> Amount ti) -- ^ buyti_ek
  -> Amount tj -- ^ amount of tj tokens we have to spend on ek
  -> Amount ti -- ^ ti tokens to sell on el
  -> Amount ti -- ^ profit in ti on ek
profit_tiek _ sellti_el buyti_ek max_in_tjek in_tiel = Amount . floor $ 1/(titjel in_tiel * tjtiek (sellti_el in_tiel)) where
  --ti:tj exchange ratio for input amount in_tiel on exchange el
  --in this case, we are selling ti on el
  titjel in_tiel' = fromIntegral in_tiel' / fromIntegral (sellti_el in_tiel')
  --tj:ti exchange ratio for input amount tiek_in on exchange ek
  -- in this case, we are buying ti on ek
  -- note that the denominator becomse constant if in_tjek' exceeds max amount of tj on ek (max_in_tjek)
  tjtiek in_tjek' = fromIntegral (buyti_ek in_tjek') / max (fromIntegral in_tjek') (fromIntegral max_in_tjek)

-- TODO improve this to search multiple possible local maxima
-- | find the maximum of a function numerically
searchMax :: (Show a, Show b,  NFData b, Integral a, Ord b) =>
  [Int] -- ^ search resolution for each iteration
  -> (a,a) -- ^ search domain
  ->  (a->b) -- ^ function to search
  -> a -- ^ max value
searchMax [] (mn,mx) f = if f mn > f mx then mn else mx
searchMax (n':ns) (mn,mx) f = r where
  -- first split the domain including boundary points
  step' = (mx-mn) `div` fromIntegral n'
  (n,step) = if step' == 0 then (length range, 1) else (n',step') where
    range = [mn..mx]
  pts = [mn+step*fromIntegral i | i<-[0..n]]
  -- compute values in parallel
  vals = parMap rdeepseq f pts
  -- find the maximum value
  (_,maxp) = foldl1 (\(m,mp) (x,p) -> if x > m then (x,p) else (m,mp)) (zip vals pts)
  -- construct the new search domain and recurse
  back = max mn (maxp-step)
  front = min mx (maxp+step)
  r = if step == 1 then maxp else searchMax ns (back, front) f

-- DELETE
-- | runs profit_tiek
maximize_profit_tiek ::
  (Token ti, Token tj, Exchange el, Exchange ek)
  => Proxy (ti, tj, el, ek) -- ^ proxy to help make function name "type bindings" explicit
  -> Amount ti -- ^ ti balance on el
  -> Amount tj -- ^ tj balance on ek
  -> (Amount ti -> Amount tj) -- ^ sellti_el
  -> (Amount tj -> Amount ti) -- ^ buyti_ek
  -> (Amount ti, Amount tj) -- ^ amount ti to sell on el and amount tj to sell on ek
maximize_profit_tiek b_tiel b_tjek sellti_el buyti_ek = undefined

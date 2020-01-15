{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Potato.CryptoTrader.Exchanges.Bilaxy.Exchange (
  RealBilaxyPair(..),
  Bilaxy(..),
  BilaxyCache,
  BilaxyCtx,
  BilaxyOrderDetails(..)
) where

import           Control.Exception
import           Control.Monad                                (foldM, forM)
import           Control.Monad.IO.Class
import           Data.List                                    (mapAccumL)
import           Data.Proxy
import           Potato.CryptoTrader.Exchanges.Bilaxy.Account
import qualified Potato.CryptoTrader.Exchanges.Bilaxy.Aeson   as BA
import           Potato.CryptoTrader.Exchanges.Bilaxy.Query
import           Potato.CryptoTrader.Helpers
import           Potato.CryptoTrader.Types

import           Debug.Trace

-- | class represents a real trading pair on bilaxy exchange
-- (as oppose to derived one i.e. with FlipExchange)
class (Token t1, Token t2) => RealBilaxyPair t1 t2 where
  getPairId :: Proxy t1 -> Proxy t2 -> Int

instance RealBilaxyPair TT USDT where
  getPairId _ _ = 151

-- | Bilaxy exchange type
data Bilaxy

-- | assumes bilaxy fee is fixed for all
bilaxyFee :: FeeRatio t
--bilaxyFee = FeeRatio 0.00075
-- if paying with BIA
bilaxyFee = FeeRatio 0.000375
--bilaxyFee = FeeRatio 0.0

-- TODO move to Cache.hs
type BilaxyCache = ()

-- | `ExchangeCtx Bilaxy` type used by `ExchangeT Bilaxy`
type BilaxyCtx = (BilaxyCache, BilaxyAccount)

instance Exchange Bilaxy where
  exchangeName _ = "Bilaxy"
  type ExchangePairId Bilaxy = Int
  type ExchangeCache Bilaxy = BilaxyCache
  type ExchangeAccount Bilaxy = BilaxyAccount

-- | exchange helper method for getting balance
getBalanceHelper :: forall t e m. (MonadExchange m, ExchangeToken t e) => Proxy (t,e) -> ExchangeT e m (Amount t)
getBalanceHelper p = do
  b <- liftIO $ getBalanceOf $ symbol p
  return . Amount . floor $ fromIntegral (decimals (Proxy :: Proxy t)) * b

-- | Token types
instance ExchangeToken TT Bilaxy where
  getBalance = getBalanceHelper

instance ExchangeToken USDT Bilaxy where
  getBalance = getBalanceHelper

-- | `Order t1 t2 Bilaxy` type
data BilaxyOrderDetails t1 t2 = BilaxyOrderDetails {
  orderId :: Int
} deriving (Show)


type BilaxyExchangePairConstraints t1 t2 = (RealBilaxyPair t1 t2, ExchangeToken t1 Bilaxy, ExchangeToken t2 Bilaxy)

getDepthHelper ::
  forall t1 t2 m. (BilaxyExchangePairConstraints t1 t2, MonadExchange m)
  => Proxy (t1, t2, Bilaxy)
  -> m ([(AmountRatio t2 t1, Amount t1)],[(AmountRatio t2 t1, Amount t1)]) -- ^ (asks, bids)
getDepthHelper pproxy = do
  depth <- liftIO $ getDepth (pairId pproxy)
  let
    t1d = fromInteger $ decimals (Proxy :: Proxy t1)
    t2d = fromInteger $ decimals (Proxy :: Proxy t2)
    fixDecimals = map (\(BA.MarketOrder p v _) -> (stdDenomToRatio p, fromStdDenom v))
    -- asks are people trying to sell t1 for t2
    -- bids are people trying to buy t1 with t2
    -- price is always in t2, volume in t1
    -- in this case t1 is the token being traded, and t2 is the base token
    -- TODO/NOTE I'm unsure if this interpretation is consistent accross all trading pairs. I only checked for TT/USDT
    asks = fixDecimals $ BA.asks depth
    bids = fixDecimals $ BA.bids depth
  return (asks, bids)


-- TODO do a better implementation
collapseOrderState :: [OrderState] -> OrderState
collapseOrderState []                    = Missing
collapseOrderState [x]                   = x
collapseOrderState (Pending:_)           = Pending
collapseOrderState (PartiallyExecuted:_) = PartiallyExecuted
collapseOrderState (Cancelled:_:os)      = Cancelled
collapseOrderState (Missing:_)           = Missing
collapseOrderState (Executed:o:os)       = collapseOrderState (o:Executed:os)

instance BilaxyExchangePairConstraints t1 t2 => ExchangePair t1 t2 Bilaxy where
  pairId _ = getPairId (Proxy :: Proxy t1) (Proxy :: Proxy t2)

  -- TODO finish... Could include exchange pair id but it's encoded in the type so idk :\
  type Order t1 t2 Bilaxy = [BilaxyOrderDetails t1 t2]

  getStatus _ orders = foldM foldOrders (defOrderStatus {orderState = Executed}) orders where
    foldOrders osacc (BilaxyOrderDetails oid) = do
      case orderState osacc of
        -- something went wrong in our query, just abort
        Missing -> return osacc
        -- if one order is cancelled, we say the whole thing is (no PartiallyCancelled state)
        Cancelled -> return osacc
        -- Assuming orders are in order, one PartiallyExecuted or Pending order means the rest should be in pending state
        -- TODO you can check and log an error if it's not the case
        x | x == PartiallyExecuted || x == Pending -> return osacc
        Executed -> do
          v <- liftIO $ try (getOrderInfo oid)
          case v of
            -- something went wrong, just return the `Missing` state to indicate this
            Left (SomeException _) -> return $ osacc {orderState = Missing}
            Right oi               -> case BA.oi_status oi of
              BA.Cancelled -> return $ osacc {orderState = Cancelled}
              BA.NotTradedYet -> return $ osacc {orderState = Pending}
              x | x == BA.TradedCompletely || x == BA.TradedPartly -> return $
                osacc {
                  orderState = BA.toOrderState (BA.oi_status oi)
                  -- this should never change (except for the first one)
                  -- TODO you can check and log an error if it's not the case
                  , orderType = BA.oi_type oi
                  , orderAmount = newAmount
                } where
                  -- TODO omg test this!!!
                  (prevt1a,prevt2a) = orderAmount osacc
                  dt1 = (BA.oi_amount oi - BA.oi_left_amount oi)
                  dt2 = dt1 * BA.oi_price oi
                  newAmount = (prevt1a + fromStdDenom dt1, prevt2a + fromStdDenom dt2)

  canCancel _ _ = True

  cancel _ orders = all id <$> forM orders (\(BilaxyOrderDetails oid) -> do
    v <- liftIO $ try (cancelOrder oid)
    case v of
      Left (SomeException _) -> return False
      Right oi               -> return True)

  -- | N.B. this function can't distinguish between orders made by different calls to order so it groups them all together as a single order
  -- also note that this function returns orders not made through this library
  -- TODO fix this problem by using ExchangeCache
  getOrders _ = do
    orders <- liftIO $ getOrderList $ pairId (Proxy :: Proxy (t1,t2,Bilaxy))
    return $ [map (BilaxyOrderDetails . BA.oi_id) orders]

  order pproxy ofl ot t1 t2 = do
    (asks, bids) <- getDepthHelper pproxy
    let
      t1proxy = Proxy :: Proxy t1
      t2proxy = Proxy :: Proxy t2
      t1d = fromInteger $ decimals (Proxy :: Proxy t1)
      t2d = fromInteger $ decimals (Proxy :: Proxy t2)
      pair = pairId pproxy
    v <- case ofl of
      Rigid -> do
        let
          amount_t1 = fromIntegral t1 / fromIntegral (decimals t1proxy)
          amount_t2 = fromIntegral t2 / fromIntegral (decimals t2proxy)
          price_t2 = amount_t2 / amount_t1
        return <$> liftIO (try (postOrder pair amount_t1 price_t2 ot))
      Flexible -> do
        let
          pvpairs = case ot of
            -- buying t1 with t2
            Buy  -> make_buyPerPricet1_from_askst1 asks t2
            -- selling t1 for t2
            Sell -> make_toSellPerPricet1_from_bidst1 bids t1
          convertedPairs = (flip map) pvpairs $ \(pt2t1, vt1) -> (ratioToStdDenom pt2t1, toStdDenom vt1)
        forM convertedPairs $ \(pt2t1, vt1) -> liftIO $ try (postOrder pair vt1 pt2t1 ot)
    forM v $ \case
      Left (SomeException e) -> do
        liftIO $ print e
        return undefined
      Right oid -> return $ BilaxyOrderDetails oid

  getExchangeRate pproxy = do
    (asks, bids) <- getDepthHelper pproxy
    let
      sellt1 = applyFee bilaxyFee . make_sellt1_from_bidst1 bids
      buyt1 = applyFee bilaxyFee . make_buyt1_from_askst1 asks
      variance = undefined
    return $ ExchangeRate sellt1 buyt1 variance

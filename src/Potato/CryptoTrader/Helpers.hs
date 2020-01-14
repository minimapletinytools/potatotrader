module Potato.CryptoTrader.Helpers (
  cancelAllOrders,
  make_toSellPerPricet1_from_bidst1,
  make_buyPerPricet1_from_askst1,
  make_sellt1_from_bidst1,
  make_buyt1_from_askst1
) where

import           Data.List                 (mapAccumL)
import           Data.Proxy
import           Potato.CryptoTrader.Types




-- TODO move to a different file
-- | cancels all unexecuted or partially executed orders
cancelAllOrders :: (ExchangePair t1 t2 e, MonadExchange m) => Proxy (t1, t2, e) -> ExchangeT e m ()
cancelAllOrders p = do
  orders <- getOrders p
  mapM_ (cancel p) orders



-- | takes a list of market order bids for t1 (in base denomination)
-- (bids are people trying to buy t1 using t2)
-- and creates the toSellt1 function that shows how much t1 should be sold at each price
make_toSellPerPricet1_from_bidst1 ::
  [(AmountRatio t2 t1, Amount t1)] -- ^ list of market asks
  -> Amount t1 -- ^ amount of t1 to be sold
  -> [(AmountRatio t2 t1, Amount t1)] -- ^ amount of t1 sold at each price
make_toSellPerPricet1_from_bidst1 bids t1 = r where
  myFunc :: Amount t1 -> (AmountRatio t2 t1, Amount t1) -> (Amount t1, Amount t1)
  myFunc remainingt1 (price, volume) = (remainingt1-soldt1, soldt1) where
    soldt1 = min remainingt1 volume
  (_, soldt1Array) = mapAccumL myFunc t1 bids
  r = zip (map fst bids) (takeWhile (> 0) soldt1Array)

-- | takes a list of market order asks for t1 (in base denomination)
-- (asks are people trying to sell t2 for t1)
-- and creates the buyt1 function that shows how much t1 should be bought for a given quantity of t2
make_buyPerPricet1_from_askst1 ::
  [(AmountRatio t2 t1, Amount t1)] -- ^ list of market bids
  -> Amount t2 -- ^ amount of t2 to be sold
  -> [(AmountRatio t2 t1, Amount t1)] -- ^ amount of t1 bought at each price
make_buyPerPricet1_from_askst1 asks t2 = r where
  myFunc :: Amount t2 -> (AmountRatio t2 t1, Amount t1) -> (Amount t2, Amount t1)
  myFunc remainingt2 (price, volume) = (remainingt2-paidt2, boughtt1) where
    boughtt1 = min (remainingt2 /$:$ price) volume
    paidt2 = boughtt1 *$:$ price
  (_, boughtt1Array) = mapAccumL myFunc t2 asks
  r = zip (map fst asks) (takeWhile (> 0) boughtt1Array)

-- | helper method to create sellt1 function in `ExchangeRate`
-- see `make_toSellPerPricet1_from_bidst1`
make_sellt1_from_bidst1 :: [(AmountRatio t2 t1, Amount t1)] -> Amount t1 -> Amount t2
make_sellt1_from_bidst1 bids = sum . map (\(pt2t1, vt1) -> pt2t1 $:$* vt1) . make_toSellPerPricet1_from_bidst1 bids

-- | helper method to create buyt1 function in `ExchangeRate`
-- see `make_buyPerPricet1_from_askst1`
make_buyt1_from_askst1 :: [(AmountRatio t2 t1, Amount t1)] -> Amount t2 -> Amount t1
make_buyt1_from_askst1 asks = sum . map snd . make_buyPerPricet1_from_askst1 asks

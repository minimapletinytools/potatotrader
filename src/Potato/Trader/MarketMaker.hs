{-# LANGUAGE ConstraintKinds #-}

module Potato.Trader.MarketMaker (
  marketMaker
) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                 as T
import           Data.Time.Clock
import           Potato.Trader.Helpers
import           Potato.Trader.Types

--type MarketMakerConstraints t1 t2 e = (Token t1, Token t2, Exchange e, ExchangePair t1 t2 e)


-- TODO
--doMarketMaker :: forall t1 t2 e w m. (MarketMakerConstraints t1 t2 e, MonadIO m, MonadWriter w m) => Proxy (t1, t2, e) -> m ()
--doMarketMaker proxy = undefined




-- | logging type for market maker
-- TODO
data MarketMakerLogs = MarketMakerLogs deriving (Show)
instance Semigroup MarketMakerLogs where
  (<>) = const
instance Monoid MarketMakerLogs where
  mempty = MarketMakerLogs

--tellShow :: (Show a, MonadWriter [T.Text] m) => a -> m ()
--tellShow x = tell [T.pack (show x)]

tellString :: (MonadWriter [T.Text] m) => String -> m ()
tellString s = tell [T.pack s]

data MarketMakerParams t1 t2 = MarketMakerParams {
    orderMinMax       :: (Amount t1, Amount t2) -- the min and max amount of t1 we'll buy at once in market making
    , minProfitMargin :: AmountRatio t1 t1
  }


type MarketMakerConstraints t1 t2 e m = (ExchangePair t1 t2 e, MonadExchange m)

-- |
-- assumes t1 is the security and minimizes inventory risk of t1 by buying first then selling
marketMaker :: forall t1 t2 e m. (MarketMakerConstraints t1 t2 e m, MonadWriter [T.Text] (ExchangeT e m))
  => Proxy (t1, t2, e)
  -> MarketMakerParams t1 t2
  -> ExchangeT e m ()
marketMaker _ params = do
  startTime <- liftIO getCurrentTime
  tellString $ "BEGIN MARKET MAKER: " ++ show startTime

  -- query and cancel all orders
  qncresult <- try $ cancelAllOrders (Proxy :: Proxy (t1,t2,e))
  case qncresult of
    Left (SomeException e) -> tellString $ "exception when cancelling orders: " ++ show e
    Right _                -> return ()

  -- query balances
  gbresult <- try $ do
    t1 <- getBalance (Proxy :: Proxy (t1, e))
    t2 <- getBalance (Proxy :: Proxy (t2, e))
    return (t1, t2)
  (b_t1, b_t2) <- case gbresult of
    Left (SomeException e) -> do
      tellString $ "exception when querying balances: " ++ show e
      throwM e
    Right r                -> return r

  tellString $ "BALANCES: " ++ show (b_t1, b_t2)

  -- query exchange rate
  erresult <- try $ getExchangeRate (Proxy :: Proxy (t1,t2,e))
  exchRate <- case erresult of
    Left (SomeException e) -> do
      tellString $ "exception when querying exchange rate: " ++ show e
      throwM e
    Right r                -> return r

  let
    (orderMin, orderMax) = orderMinMax params
    -- get highest buy price and lowest sell price
    -- basic version just gets the price for a small order
    -- vulnerable to small orders aimed and reducing apparent spread
    -- a better version should use a dynamic order amount (based on avg order size say) to determine which price to use for computing spread
    sampleSell_t1 = (orderMin `div` 4)
    sampleBought_t2 = (sellt1 exchRate) sampleSell_t1
    lowestSellPrice = makeRatio sampleBought_t2 sampleSell_t1 :: AmountRatio t2 t1
    sampleSell_t2 = sampleBought_t2
    sampleBought_t1 = (buyt1 exchRate) sampleSell_t2
    highestBuyPrice = makeRatio sampleSell_t2 sampleBought_t1 :: AmountRatio t2 t1

    -- compute spread
    spread = AmountRatio $ 1 - lowestSellPrice $:$/$:$ highestBuyPrice :: AmountRatio t1 t1
    minSpread = minProfitMargin params
    --minSpread = AmountFee 0.001 :: AmountFee t2

  -- TODO
  -- only proceed if spread - fee is over our min profit margin

  let
    slippage_t1 = AmountRatio 0.99 :: AmountRatio t1 t1
    slippage_t2 = AmountRatio 0.99 :: AmountRatio t2 t2
    makerAmount = fromStdDenom 10 :: Amount t2 -- this is hardcoded to USDT units TODO make it a parameter
  -- assuming exchange rate function is monotically increasing/decreasing, mid point search to our slippage tolerance
  -- actual purchase amount is min of (t2 balance, makerAmount, slippage threshold)
  -- place t1 buy order
  -- begin loop: checkBuy
    -- q1 <- query market
    -- if lowest sell price has reduced spread to be under our profit margin
      -- cancel all orders and start over
    -- query order status
    -- if partially executed or executed
      -- put in sell order of executed amount at price just below lowest sell price in q1
      -- start loop thread: checkSell
        -- query market
        -- if lower sell price is put in
          -- if it reduces our spread to be under our profit margin
            -- log a message
          -- put in lower sell price
      -- if fully executed
        -- join all checkSell
        -- exit
  return ()

{-# LANGUAGE ConstraintKinds #-}

module Potato.CryptoTrader.MarketMaker (
  marketMaker
) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                   as T
import           Data.Time.Clock
import           Potato.CryptoTrader.Helpers
import           Potato.CryptoTrader.Types

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



-- |
-- assumes t1 is the security and minimizes inventory risk of t1 by buying first then selling
marketMaker :: forall t1 t2 e m. (ExchangePair t1 t2 e, MonadWriter [T.Text] (ExchangeT e m)) =>
  Proxy (t1, t2, e)
  -> ExchangeT e m ()
marketMaker _ = do
  undefined
  {-

  startTime <- liftIO getCurrentTime
  tellString $ "BEGIN MARKET MAKER: " ++ show startTime

  -- query and cancel all orders
  qncresult <- try cancelAllOrders (Proxy :: Proxy (t1,t2,e1))
  case qncresult of
    Left (SomeException e) -> tellString $ "exception when cancelling orders: " ++ show e
    Right _                -> return ()

  -- query balances
  gbresult <- try $ do
    t1 <- lifte1 $ getBalance (Proxy :: Proxy (t1, e1))
    t2 <- lifte1 $ getBalance (Proxy :: Proxy (t2, e1))
    return (t1, t2)
  (b_t1, b_t2) <- case gbresult of
    Left (SomeException e) -> do
      tellString $ "exception when querying balances: " ++ show e
      throwM e
    Right r                -> return r

  tellString $ "BALANCES: " ++ show (b_t1, b_t2)

  -- get highest buy price and lowest sell price
  -- compute spread
  spread = 0 :: AmountRatio t2 t1
  let
    minProfitMargin = AmountFee 0.001 :: AmountFee t2
  -- only proceed if spread - fee is over our min profit margin
  let
    slippage_t1 = AmountFee 0.99 :: AmountFee t1
    slippage_t2 = AmountFee 0.99 :: AmountFee t2
    makerAmount = fromStdDenom 10 -- this is hardcoded to USDT units TODO make it a parameter
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
-}

{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE ConstraintKinds #-}

module Potato.Trader.MarketMaker (
  marketMaker,
  MarketMakerParams(..),
  MarketMakerConstraints,
) where

import           Control.Concurrent              (threadDelay)
import qualified Control.Concurrent.Thread.Group as TG
import           Control.Exception               (AssertionFailed (..))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                       as T
import           Data.Time.Clock
import           Potato.Trader.Helpers
import           Potato.Trader.Types

import           Debug.Trace


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


modifyRatio :: AmountRatio t1 t2 -> AmountRatio t1 t1 -> AmountRatio t1 t2
modifyRatio (AmountRatio t1t2) (AmountRatio t1t1) = AmountRatio $ t1t2*t1t1


calcSpread_ :: forall t1 t2.
  AmountRatio t2 t1 -- ^ highest bid
  -> AmountRatio t2 t1 -- ^ lowest ask
  -> AmountRatio t2 t2 -- ^ spread as a ratio increase between lowest ask and highest bid
calcSpread_ highestBidPrice lowestAskPrice = AmountRatio $ 1 - highestBidPrice $:$/$:$ lowestAskPrice

calcSpread :: forall t1 t2.
  ExchangeRate t1 t2
  -> Amount t1 -- ^ sample order size to compute spread
  -> (AmountRatio t2 t2, AmountRatio t2 t1, AmountRatio t2 t1) -- ^ (spread, highest bid price, lowest ask price)
calcSpread exchRate sample_t1 = r where
  -- N.B. this is a little weird because of the way 'ExchangeRate' works
  -- a better way to do this is to solve for `x` in `sample_t1 = (buyt1 exchRate) x
  -- and then compute lowestAskPrice as `x / sample_t1`
  -- instead we assume spread is small such that `(buyt1 exchRate) ((sellt1 exchRate) sample_t1) ~= sample_t1`
  sampleBidInput_t1 = sample_t1
  sampleBidOutput_t2 = sellt1 exchRate sampleBidInput_t1
  highestBidPrice = makeRatio sampleBidOutput_t2 sampleBidInput_t1
  sampleAskInput_t2 = sampleBidOutput_t2
  sampleAskOutput_t1 = buyt1 exchRate sampleAskInput_t2
  lowestAskPrice = makeRatio sampleAskInput_t2 sampleAskOutput_t1
  spread = calcSpread_ highestBidPrice lowestAskPrice
  r = trace (show highestBidPrice ++ " " ++ show lowestAskPrice) $ (spread, highestBidPrice, lowestAskPrice)

data MarketMakerParams t1 t2 = MarketMakerParams {
    -- N.B. in practice, min order is used to sample market spread and max order is how much we arbitrage
    -- instead, the above should be determined dynamically from the market and bounded by these values
    orderMinMax       :: (Amount t1, Amount t1) -- the min and max amount of t1 we'll buy at once in market making
    , minProfitMargin :: AmountRatio t2 t2 -- the minimum price ratio between bid and ask (spread) that is needed to attempt market making
    , makerMargin     :: (AmountRatio t2 t2, AmountRatio t2 t2) -- the amount to increase/decrease our bid/ask price when market making. Make sure the sum of this is less than minProfitMargin
  }


type MarketMakerConstraints t1 t2 e m = (ExchangePair t1 t2 e, MonadExchange m)
type MarketMakerConstraints_ t1 t2 e m = (MarketMakerConstraints t1 t2 e m, MonadWriter [T.Text] (ExchangeT e m))


-- TODO test
-- | runs market maker routine once (buys t1 once and sells all t1 bought)
-- exits when all bought tokens are sold
-- assumes t1 is the security and minimizes inventory risk of t1 by bidding first then asking
marketMaker :: forall t1 t2 e m. (MarketMakerConstraints_ t1 t2 e m)
  => Proxy (t1, t2, e)
  -> MarketMakerParams t1 t2
  -> ExchangeT e m ()
marketMaker pproxy params = do

  -- check that our parameters are good
  let
    (mmBuy, mmSell) = makerMargin params
    good = mmBuy + mmSell < minProfitMargin params
  case good of
    False -> do
      tellString $ "bad parameters"
      throwM $ AssertionFailed "bad parameters"
    True -> return ()

  startTime <- liftIO getCurrentTime
  tellString $ "BEGIN MARKET MAKER: " ++ show startTime

  -- query and cancel all orders
  {-
  qncresult <- try $ cancelAllOrders (Proxy :: Proxy (t1,t2,e))
  case qncresult of
    Left (SomeException e) -> do
      tellString $ "exception when cancelling orders: " ++ show e
      throwM e
    Right _                -> return ()
  -}

  -- query balances
  -- TODO no need to do error checking, it will be handled from outside
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
  -- TODO no need to do error checking, it will be handled from outside
  erresult <- try $ getExchangeRate pproxy False
  exchRate <- case erresult of
    Left (SomeException e) -> do
      --tellString $ "exception when querying exchange rate: " ++ show e
      throwM e
    Right r                -> return r

  let
    (orderMin, orderMax) = orderMinMax params
    -- basic version to calculate spread just gets the price for a small order
    -- which is vulnerable to small orders aimed and reducing apparent spread
    -- a better version should use a dynamic order amount (based on avg order size say) to determine which price to use for computing spread
    -- TODO use average order size over last 24 hours here instead
    (spread, highestBidPrice, _) = calcSpread exchRate (orderMin `div` 4)
    minSpread = minProfitMargin params

  -- helpful warning message
  when (spread <= 0) $ tellString "negative spread, are perhaps you are trying to do run this on an algorithmic market maker exchange like uniswap? 😂"

  -- proceed only if spread is above our minimum
  if spread < minSpread then
    tellString $ "spread too low, spread: " ++ show spread ++ " minSpread: " ++ show minSpread
  else do
    ctx <- ask
    let
      bidPrice = modifyRatio highestBidPrice (1+mmBuy)
      bidAmount = min orderMax (b_t2 /$:$ bidPrice)

    when (bidAmount < orderMin) $ throwM (AssertionFailed "insufficient balance to arbitrage")

    trace ("making order for " ++ show bidAmount ++ " at " ++ show bidPrice) $ return ()

    -- place a Rigid t1 buy order
    mo <- try $ orderByPrice pproxy Rigid Buy bidPrice bidAmount
    case mo of
      Left (SomeException e) -> do
        tellString $ "failed to make order: " ++ show e
        throwM e
      Right origBuyOrder -> do
        tg <- liftIO TG.new
        checkBuy tg origBuyOrder 0 where
          -- | loop to check our buy status
          checkBuy :: TG.ThreadGroup -> Order t1 t2 e -> Amount t1 -> ExchangeT e m ()
          checkBuy tg buyOrder alreadySelling = do
            liftIO . putStrLn $ "checking buy order status, amount selling: " ++ show alreadySelling
            curExchRate <- getExchangeRate pproxy False

            let
              cancelBid = do
                didCancel <- cancel pproxy buyOrder
                liftIO . putStrLn $ "canceled bid"
                tellString $ "cancelled bid: " ++ show didCancel
              (_, highestBidPrice, lowestAskPrice) = calcSpread exchRate (orderMin `div` 4)
              spreadFromPrevBid = calcSpread_ bidPrice lowestAskPrice

            -- get the order status
            OrderStatus os _ _ (exect1,_) <- getStatus pproxy buyOrder
            when (os == Executed || os == PartiallyExecuted) $ do
              -- TODO make sure that the sell amount is above the minimum sell amount (if not, just don't sell w/e)
              liftIO . putStrLn $ ("selling at " ++ show (modifyRatio lowestAskPrice (1-mmSell)) ++ " " ++ show (exect1-alreadySelling))
              -- how to do error case here?
              -- put in sell order of executed amount at price just below lowest sell price in q1
              makerSell <- orderByPrice pproxy Rigid Sell (modifyRatio lowestAskPrice (1-mmSell)) (exect1-alreadySelling)
              -- start a loop to check our sell status
              liftIO $ TG.forkIO tg (flip runReaderT ctx $ checkSell makerSell)
              return ()

            when (os == Executed) $ tellString "buy order fully executed, waiting for sell orders to finalize"

            -- check if there are still profits to be made on our original buy order
            (continue, newBuyOrder) <- do
              -- if lowest ask price has reduced spread to be under our profit margin
              if spreadFromPrevBid < minSpread then do
                liftIO . putStrLn $ "cancel bid 1"
                tellString $ "ask price dropped and brought spread below threshold, spread: " ++ show spreadFromPrevBid ++ ", minSpread: " ++ show minSpread
                cancelBid
                return (False, buyOrder)
              -- if highest bid price has passed our bid
              else if highestBidPrice > bidPrice then do
                liftIO . putStrLn $ "cancel bid 2"
                tellString $ "bid price passed maker bid: " ++ show bidPrice ++ ", new highest bid price: " ++ show highestBidPrice
                cancelBid
                return (False, buyOrder)
              --else if
                -- TODO if highestBidPrice has gone down enough, cancel order and make a new one
              else
                return (True, buyOrder)

            -- restart checkBuy or wait for all checkSells to finish
            if os == Executed || os == Cancelled || not continue then do
              liftIO . putStrLn $ "zzzzzzz"
              liftIO $ TG.wait tg
            else do
              -- wait 1/5th of a second
              -- TODO parameterize
              liftIO $ threadDelay (floor 2e5)
              checkBuy tg newBuyOrder exect1

          -- | loop to check our sell order status
          -- we need to fork this so we force the inner monad to IO
          checkSell :: Order t1 t2 e -> ExchangeT e IO ()
          checkSell sellOrder = do
            curExchRate <- getExchangeRate pproxy False
            let (_, _, lowestAskPrice) = calcSpread exchRate (orderMin `div` 4)
            OrderStatus os ot (origt1, origt2) exec <- getStatus (Proxy :: Proxy (t1,t2,e)) sellOrder
            case os of
              Executed -> return ()
              x | x == PartiallyExecuted || x == Pending -> do
                -- if lower sell price is put in
                if lowestAskPrice < makeRatio origt2 origt1 then do
                  -- cancel sell order
                  didCancel <- cancel pproxy sellOrder
                  liftIO $ putStrLn $ "cancelled bid: " ++ show didCancel
                  -- TODO better logging here...
                  when (calcSpread_ bidPrice lowestAskPrice < minSpread) $
                    liftIO $ putStrLn $ "warning: ask price has dropped below spread threshold"
                  -- still put in lower sell price to liquidate our inventory
                  newSellOrder <- orderByPrice pproxy Rigid Sell (modifyRatio lowestAskPrice (1-mmSell)) origt1
                  checkSell newSellOrder
                --else if
                  -- TODO if lowestAskPrice has gone up enough, cancel order and make a new one
                else
                  checkSell sellOrder
              _ -> return ()
  return ()

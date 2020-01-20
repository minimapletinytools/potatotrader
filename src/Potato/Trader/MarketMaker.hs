{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ConstraintKinds     #-}

module Potato.Trader.MarketMaker (
  marketMaker,
  MarketMakerParams(..),
  MarketMakerConstraints,
) where

import           Control.Concurrent              (myThreadId, threadDelay)
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
  --r = trace (show highestBidPrice ++ " " ++ show lowestAskPrice) $ (spread, highestBidPrice, lowestAskPrice)
  r = (spread, highestBidPrice, lowestAskPrice)

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

  -- load up params
  let
    (orderMin, orderMax) = orderMinMax params
    minSpread = minProfitMargin params
    (mmBuy, mmSell) = makerMargin params

  -- check that our parameters are good
  case mmBuy + mmSell < minProfitMargin params of
    False -> do
      tellString $ "bad parameters"
      throwM $ AssertionFailed "bad parameters"
    True -> return ()

  startTime <- liftIO getCurrentTime
  tellString $ "BEGIN MARKET MAKER: " ++ show startTime

  -- query and cancel all orders
  qncresult <- try $ cancelAllOrders (Proxy :: Proxy (t1,t2,e))
  case qncresult of
    Left (SomeException e) -> do
      tellString $ "exception when cancelling orders: " ++ show e
      throwM e
    Right _                -> return ()

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

  exchRate <- getExchangeRate pproxy False
  let
    -- basic version to calculate spread just gets the price for a small order
    -- which is vulnerable to small orders aimed and reducing apparent spread
    -- a better version should use a dynamic order amount (based on avg order size say) to determine which price to use for computing spread
    -- TODO use average order size over last 24 hours here instead
    (spread, highestBidPrice, lowestAskPrice) = calcSpread exchRate (orderMin `div` 4)

  -- helpful warning message
  when (spread <= 0) $ liftIO $ putStrLn "negative spread, are perhaps you are trying to do run this on an algorithmic market maker exchange like uniswap? 😂"

  -- proceed only if spread is above our minimum
  if spread < minSpread then
    liftIO $ putStrLn $ "spread too low, spread: " ++ show spread ++ " minSpread: " ++ show minSpread
  else do

    liftIO $ putStrLn $ "market making potential (bid, ask, spread): " ++ show (spread, highestBidPrice, lowestAskPrice)

    -- TODO wrap this all up in a function..
    let
      bidPrice = modifyRatio highestBidPrice (1+mmBuy)
      bidAmount = min orderMax (b_t2 /$:$ bidPrice)
    when (bidAmount < orderMin) $ throwM (AssertionFailed "insufficient balance to arbitrage")
    liftIO . putStrLn $ "making bid order for " ++ show bidAmount ++ " at " ++ show bidPrice
    mo <- try $ orderByPrice pproxy Rigid Buy bidPrice bidAmount
    case mo of
      Left (SomeException e) -> do
        tellString $ "failed to make order: " ++ show e
        throwM e
      Right bidOrder -> do
        tg <- liftIO TG.new
        checkBuy pproxy params tg bidOrder 0
  return ()


modifyExchangeRateFunction ::
  (Amount t1 -> Amount t2) -- ^ original function
  -> Amount t1 -- ^ amount to remove
  -> (Amount t1 -> Amount t2) -- ^ modified function
modifyExchangeRateFunction old_st1 remove_t1 = new_st1 where
  removed_t2 = old_st1 remove_t1
  new_st1 t = old_st1 (t+remove_t1) - removed_t2

-- | loop to check our buy status
checkBuy :: forall t1 t2 e m. (MarketMakerConstraints_ t1 t2 e m)
  => Proxy (t1, t2, e)
  -> MarketMakerParams t1 t2
  -> TG.ThreadGroup -- ^ thread group used to track checkSell loops
  -> Order t1 t2 e -- ^ the bid order we are checking
  -> Amount t1 -- ^ the amount of the bid order we've already bought and place ask orders for (needed for 'PartiallyExecuted' orders)
  -> ExchangeT e m ()
checkBuy pproxy params tg bidOrder alreadyAsking = do
  -- load up params
  let
    greedReorderMargin = 1.5
    (orderMin, orderMax) = orderMinMax params
    minSpread = minProfitMargin params
    (mmBuy, mmSell) = makerMargin params

  --liftIO . putStrLn $ "checking buy order status, amount selling: " ++ show alreadyAsking


  OrderStatus os _ (origt1, origt2) (exect1, exect2) <- getStatus pproxy bidOrder
  curExchRate' <- getExchangeRate pproxy False

  let
    -- recompute the original bid price
    -- N.B. due to exchange roundoff issue, this may not actually the same price as the order that went through
    bidPrice = makeRatio origt2 origt1

    -- remove our own order from the exchange rate function
    modified_buyt1 = modifyExchangeRateFunction (buyt1 curExchRate') (origt2 - exect2)
    curExchRate = curExchRate' { buyt1 = modified_buyt1 }

    -- calculate spread
    (newSpread, highestBidPrice, lowestAskPrice) = calcSpread curExchRate (orderMin `div` 4)
    prevHighestBidPrice = modifyRatio bidPrice (1/(1+mmBuy))
    spreadFromPrevBid = calcSpread_ prevHighestBidPrice lowestAskPrice

  -- if order was executed, put in a sell order
  when (os == Executed || os == PartiallyExecuted) $ do
    let
      -- If the order was previously 'PartiallyExecuted' some of the bought t1 from this order has already been sold so substract this amount
      askAmount = (exect1-alreadyAsking)
      askPrice = (modifyRatio lowestAskPrice (1-mmSell))
    -- TODO make sure that the ask amount is above the minimum ask amount (if not, just don't sell w/e)
    -- only make a ask order if we actually bought more
    when (askAmount > 0) $ do
      liftIO . putStrLn $ "selling at " ++ show askPrice ++ " " ++ show askAmount
      -- put in sell order of executed amount at price just below lowest sell price
      askOrder <- orderByPrice pproxy Rigid Sell askPrice askAmount
      -- FUTURE handle order exceptions here
      -- start a loop to check our sell status
      ctx <- ask
      liftIO $ TG.forkIO tg (flip runReaderT ctx $ checkSell pproxy params bidPrice askOrder 0)
      -- helpful logging message
      when (os == Executed) $
        liftIO $ putStrLn $ "bid order fully executed"

  -- if order was not fully executed, check if we need to put in a new one
  (continue, newBidOrder) <- if os == PartiallyExecuted || os == Pending then do
    reorder <-
      if spreadFromPrevBid < minSpread then do
        liftIO $ putStrLn $ "ask price dropped and brought spread below threshold, spread: " ++ show spreadFromPrevBid ++ ", minSpread: " ++ show minSpread
        return True
      -- if highest bid price has passed our bid
      -- N.B. due to the "exchange round off" issue, it's common for the bid we put in ourselves to raise the highestBidPrice over our margin
      -- TODO fix it ^
      else if highestBidPrice > bidPrice then do
        liftIO $ putStrLn $ "bid price passed our bid: " ++ show bidPrice ++ ", new highest bid price: " ++ show highestBidPrice
        return True
      -- if highestBidPrice has gone down enough, cancel order and make a new one
      else if bidPrice > modifyRatio highestBidPrice (1+mmBuy*greedReorderMargin) then do
        liftIO $ putStrLn $ "highest bid price has down enough, making new bid order"
        return True
      else
        return False

    if reorder then do
      didCancel <- cancel pproxy bidOrder
      -- if we didn't manage to cancel, order must have gone through, the next loop will take care of it
      if not didCancel then do
        liftIO $ putStrLn $ "failed to cancel bid"
        return (True, bidOrder)
      else do
        liftIO $ putStrLn $ "cancelled bid"
        if newSpread < minSpread then do
          let
            newBidPrice = modifyRatio highestBidPrice (1+mmBuy)
            newBidAmount = origt1 - exect1
          -- TODO make sure newBidAmount is not below our minimum bid amount of the exchange
          liftIO $ putStrLn $ "making order for " ++ show newBidAmount ++ " at " ++ show newBidPrice
          newBidOrder <- orderByPrice pproxy Rigid Buy newBidPrice newBidAmount
          return (True, newBidOrder)
        else do
          -- new spread is no longer profitable, wait for all checkSells to finish and quit
          liftIO $ putStrLn $ "new spread no longer profitable"
          return (False, undefined)
    else
      return (True, bidOrder)
  -- order went through or messed up, wait for all checkSells to finish and quit
  else
    return (False, undefined)

  -- loop or exit
  if continue then do
    -- wait 1/5th of a second
    -- TODO parameterize
    liftIO $ threadDelay (floor 2e5)
    checkBuy pproxy params tg newBidOrder exect1
  else do
    liftIO $ putStrLn "checkBuy done, waiting for all checkSells to join"
    liftIO $ TG.wait tg
    return ()

-- | loop to check our sell order status
-- we need to fork this so we force the inner monad to IO
checkSell :: forall t1 t2 e. (MarketMakerConstraints t1 t2 e IO)
  => Proxy (t1, t2, e)
  -> MarketMakerParams t1 t2
  -> AmountRatio t2 t1 -- ^ the bid price we paid for the inventory we are selling here
  -> Order t1 t2 e -- ^ the ask order we are checking
  -> Amount t1 -- ^ the amount of the ask order that's already been bought (needed for 'PartiallyExecuted' orders)
  -> ExchangeT e IO ()
checkSell pproxy params bidPrice askOrder alreadySold = do
  -- load up params
  let
    greedReorderMargin = 1.5
    (orderMin, orderMax) = orderMinMax params
    minSpread = minProfitMargin params
    (mmBuy, mmSell) = makerMargin params

  tid <- liftIO myThreadId

  OrderStatus os _ (origt1, origt2) (exect1, _) <- getStatus (Proxy :: Proxy (t1,t2,e)) askOrder

  curExchRate' <- getExchangeRate pproxy False
  let
    -- recompute the original ask price
    -- N.B. due to exchange roundoff issue, this may not actually the same price as the order that went through
    askPrice = makeRatio origt2 origt1

    -- remove our own order from the exchange rate function
    modified_sellt1 = modifyExchangeRateFunction (sellt1 curExchRate') (origt1 - exect1)
    curExchRate = curExchRate' { sellt1 = modified_sellt1 }

    -- calculate spread
    -- N.B it's possible this new spread calculation includes our own 'PartiallyExecuted' buy order in it (not a big deal and unlikely to happen, but worth mentioning)
    (_, _, lowestAskPrice) = calcSpread curExchRate (orderMin `div` 4)

  -- TODO this should substract tx fees in report
  -- log how much we made
  when (exect1 - alreadySold > 0) $
    liftIO $ putStrLn $ show tid ++ ": sold " ++ show (alreadySold - exect1) ++ " at " ++ show askPrice ++ " over " ++ show bidPrice ++ " for a profit of " ++ show ((askPrice - bidPrice) $:$* (alreadySold - exect1))

  -- If order has not gone through, check it's status and reorder if necessary
  when (os == PartiallyExecuted || os == Pending) $ do
    reorder <-
      -- if lower sell price is put in
      if lowestAskPrice < askPrice then do
        liftIO $ putStrLn $ show tid ++ ": lowest ask price has gone below our sell order, making new sell order"
        return True
      -- if lowest ask price has gone up enough
      else if askPrice < modifyRatio lowestAskPrice (1-mmSell*greedReorderMargin) then do
        liftIO $ putStrLn $ show tid ++ ": lowest ask price has gone up enough, making new sell order"
        return True
      else
        return False
    if reorder then do
      didCancel <- cancel pproxy askOrder
      if not didCancel then do
        -- if we didn't manage to cancel, order must have gone through, the next loop will take care of it
        liftIO $ putStrLn $ show tid ++ ": failed to cancel bid"
        checkSell pproxy params bidPrice askOrder exect1
      else do
        liftIO $ putStrLn $ show tid ++ ": cancelled ask"
        -- log a helpful message to indicate we are losing money
        when (calcSpread_ bidPrice lowestAskPrice < minSpread) $
          liftIO $ putStrLn $ show tid ++ ": warning: ask price has dropped below spread threshold (possible loss)"
        let
          askAmount = origt1 - exect1
          askPrice = modifyRatio lowestAskPrice (1-mmSell)
        -- TODO check that the new sell amount is not below the min sell amount
        when (askAmount > 0) $ do
          liftIO . putStrLn $ show tid ++ ": new sell order at " ++ show askPrice ++ " " ++ show askAmount
          newAskOrder <- orderByPrice pproxy Rigid Sell askPrice askAmount
          checkSell pproxy params bidPrice newAskOrder exect1
    else
      checkSell pproxy params bidPrice askOrder exect1

  when (os == Executed) $
    liftIO $ putStrLn $ show tid ++ ": checkSell done"

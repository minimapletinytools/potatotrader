{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE ConstraintKinds #-}

module Potato.Trader.MarketMaker (
  marketMaker
) where

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
  -- and then compute highestBidPrice as `x / sample_t1`
  -- instead we assume spread is small such that `(buyt1 exchRate) ((sellt1 exchRate) sample_t1) ~= sample_t1`
  sampleSell_t1 = sample_t1
  sampleBought_t2 = (sellt1 exchRate) sampleSell_t1
  lowestAskPrice = makeRatio sampleBought_t2 sampleSell_t1 :: AmountRatio t2 t1
  sampleSell_t2 = sampleBought_t2
  sampleBought_t1 = (buyt1 exchRate) sampleSell_t2
  highestBidPrice = makeRatio sampleSell_t2 sampleBought_t1 :: AmountRatio t2 t1
  spread = calcSpread_ highestBidPrice lowestAskPrice
  r = (spread, highestBidPrice, lowestAskPrice)

data MarketMakerParams t1 t2 = MarketMakerParams {
    -- N.B. in practice, min order is used to sample market spread and max order is how much we arbitrage
    -- instead, the above should be determined dynamically from the market and bounded by these values
    orderMinMax       :: (Amount t1, Amount t1) -- the min and max amount of t1 we'll buy at once in market making
    , minProfitMargin :: AmountRatio t2 t2 -- the minimum price ratio between bid and ask (spread) that is needed to attempt market making
    , makerMargin     :: (AmountRatio t2 t2, AmountRatio t2 t2) -- the amount to increase/decrease our bid/ask price when market making. Make sure the sum of this is less than minProfitMargin
  }

defTTUSDTMarketMakerParms :: MarketMakerParams TT USDT
defTTUSDTMarketMakerParms = MarketMakerParams {
    orderMinMax = (fromStdDenom 1000, fromStdDenom 3000)
    , minProfitMargin = AmountRatio (0.00075*3)
    , makerMargin = (AmountRatio 0.0007, AmountRatio 0.0007)
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

  -- query exchange rate
  -- TODO no need to do error checking, it will be handled from outside
  erresult <- try $ getExchangeRate pproxy
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
      bidAmount = max orderMax (b_t2 /$:$ bidPrice)

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
            curExchRate <- getExchangeRate pproxy

            let
              cancelBid = do
                didCancel <- cancel pproxy buyOrder
                tellString $ "cancelled bid: " ++ show didCancel
                return False
              (_, highestBidPrice, lowestAskPrice) = calcSpread exchRate (orderMin `div` 4)
              spreadFromPrevBid = calcSpread_ bidPrice lowestAskPrice

            -- get the order status
            OrderStatus os _ _ (exect1,_) <- getStatus pproxy buyOrder
            when (os == Executed || os == PartiallyExecuted) $ do
              -- how to do error case here?
              -- put in sell order of executed amount at price just below lowest sell price in q1
              makerSell <- orderByPrice pproxy Rigid Sell (modifyRatio lowestAskPrice (1-mmSell)) (exect1-alreadySelling)
              -- start a loop to check our sell status
              liftIO $ TG.forkIO tg (flip runReaderT ctx $ checkSell makerSell)
              return ()

            when (os == Executed) $ tellString "buy order fully executed, waiting for sell orders to finalize"

            -- check if there are still profits to be made on our original buy order
            continue <- do
              -- if lowest ask price has reduced spread to be under our profit margin
              if spreadFromPrevBid < minSpread then do
                tellString $ "ask price dropped and brought spread below threshold, spread: " ++ show spreadFromPrevBid ++ ", minSpread: " ++ show minSpread
                cancelBid
              -- if highest bid price has passed our bid
              else if highestBidPrice > bidPrice then do
                tellString $ "bid price passed maker bid: " ++ show bidPrice ++ ", new highest bid price: " ++ show highestBidPrice
                cancelBid
              else
                return True

            -- restart checkBuy or wait for all checkSells to finish
            if os == Executed || not continue then
              liftIO $ TG.wait tg
            else
              checkBuy tg buyOrder exect1

          -- | loop to check our sell order status
          -- we need to fork this so we force the inner monad to IO
          checkSell :: Order t1 t2 e -> ExchangeT e IO ()
          checkSell sellOrder = do
            curExchRate <- getExchangeRate pproxy
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
                else
                  checkSell sellOrder
              _ -> return ()
  return ()

{-# LANGUAGE ConstraintKinds #-}

module Potato.CryptoTrader.MarketMaker (
  marketMaker
) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                 as T
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

-- TODO move to a different file
-- | cancels all unexecuted or partially executed orders
cancelAllOrders :: (ExchangePair t1 t2 e, MonadExchange m) => Proxy (t1, t2, e) -> ExchangeT e m ()
cancelAllOrders p = do
  orders <- getOrders p
  mapM_ (cancel p) orders

-- |
marketMaker :: forall t1 t2 e m. (ExchangePair t1 t2 e, MonadWriter [T.Text] (ExchangeT e m)) =>
  Proxy (t1, t2, e)
  -> ExchangeT e m ()
marketMaker _ = do
  return ()

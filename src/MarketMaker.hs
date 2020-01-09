{-# LANGUAGE ConstraintKinds #-}

module MarketMaker (
  doMarketMaker
) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import           Types

type MarketMakerConstraints t1 t2 e = (Token t1, Token t2, Exchange e, ExchangePair t1 t2 e)


-- TODO
doMarketMaker :: forall t1 t2 e w m. (MarketMakerConstraints t1 t2 e, MonadIO m, MonadWriter w m) => Proxy (t1, t2, e) -> m ()
doMarketMaker proxy = undefined

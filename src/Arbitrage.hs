{-# LANGUAGE ConstraintKinds #-}

module Arbitrage (

) where

import           Data.Proxy
import           Types

type ArbitrageConstraints t1 t2 e1 e2 = (Token t1, Token t2, Exchange e1, Exchange e2, ExchangePair t1 t2 e1, ExchangePair t1 t2 e2)
doArbitrage :: (ArbitrageConstraints t1 t2 e1 e2) => Proxy (t1, t2, e1, e2) -> IO ()
doArbitrage = do
  undefined
  -- first query all existing orders

  -- cancel them

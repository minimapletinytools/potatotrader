{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs   #-}
{-# LANGUAGE InstanceSigs   #-}
-- {-# LANGUAGE AllowAmbiguousTypes   #-}
-- {-# LANGUAGE UndecidableInstances   #-}

module TokenTypes (
  Liquidity (..),
  Token (..),
  Exchange (..),
  ExchangeToken (..),
  Network(..)
) where

import Data.Proxy
import Data.Solidity.Prim.Address (Address)

data Liquidity t1 t2 = Liquidity Integer Integer

class Token t where
  tokenName :: Proxy t -> String

class Exchange e where
  exchangeName :: Proxy e -> String

class Network n where
  networkName :: Proxy n -> String
  rpc :: Proxy n -> String

class (Token t, Exchange e) => ExchangeToken t e where
  -- symbol of token on the exchange
  symbol :: Proxy (t,e) -> String
  -- multiply by this to normalize
  decimals :: Proxy (t,e) -> Int
  decimals _ = 1
  -- on-chain address if dex, 0x0 otherwise
  ethAddr :: Proxy (t,e) -> Address
  ethAddr _ = "0x0"
  -- get balance (normalized)
  getBalance :: Proxy (t,e) -> IO Integer


-- below is a WIP, lots of typing issues go away
{-
class (Token t1, Token t2) => ExchangePair t1 t2 e where
  getLiquidity :: e -> IO (Liquidity t1 t2)

data TokenExchange t1 t2 where
  TokenExchange :: (ExchangeToken t1 e1, ExchangeToken t2 e2) => TokenExchange t1 t2

instance (ExchangeToken t1 e1, ExchangeToken t2 e2) => ExchangePair t1 t2 (TokenExchange t1 t2) where
  getLiquidity :: TokenExchange t1 t2 -> IO (Liquidity t1 t2)
  getLiquidity _ = do
    b1 <- getBalance (Proxy :: Proxy (t1, e1))
    b2 <- getBalance (Proxy :: Proxy (t2, e2))
    return $ Liquidity b1 b2


data ExchangeExchange t1 t2 t3 where
  ExchangeExchange :: (ExchangePair t1 t2 e12, ExchangePair t2 t3 e23) => e12 -> e23 -> ExchangeExchange t1 t2 t3

-- broken due to GADTs not carrying scope on their type variables
-- (so the t1 t2 t3 in "data ExchangeExchange t1 t2 t3" do not match the t1 t2 t3 in th ctor)
instance (Token t1, Token t2, Token t3) => ExchangePair t1 t3 (ExchangeExchange t1 t2 t3) where
  -- this is hard because need to query exchange rate to convert t2 balance into liquidity
  -- for now, ignore t2 balance
  getLiquidity :: ExchangeExchange t1 t2 t3 -> IO (Liquidity t1 t3)
  getLiquidity (ExchangeExchange e12 e23) = do
    Liquidity l1 _ <- getLiquidity e12
    Liquidity _ l3 <- getLiquidity e23
    return $ Liquidity l1 l3
-}

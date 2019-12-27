{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BilaxyAeson (
  BilaxyResponse(..),
  BalanceData(..),
  BalanceDataMap(..),
  sortBalanceData,
  MarketOrder(..),
  MarketDepth(..),
)
where

import GHC.Generics
import Data.Maybe
import Data.Vector ((!))
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types
import           Data.Aeson.Types         ( parseMaybe, parseEither, Result(..) )

import Debug.Trace (trace)

data BilaxyResponse a = BilaxyResponse {
  code :: Int
  , brData :: a
} deriving (Show)

instance (FromJSON a) => FromJSON (BilaxyResponse a) where
  parseJSON = withObject "BilaxyResponse" $ \v -> do
    fCode :: String <- v .: "code"
    fData :: a <- v .: "data"
    return $ BilaxyResponse (read fCode) fData

data BalanceData = BalanceData {
  symbol :: Int
  , balance :: Double
  , name :: String
  , frozen :: Double
} deriving (Generic, Show)

type BalanceDataMap = M.Map String BalanceData

sortBalanceData :: [BalanceData] -> BalanceDataMap
sortBalanceData = foldl (\m bd -> M.insert (name bd) bd m) M.empty

instance FromJSON BalanceData where
  parseJSON = withObject "BalanceData" $ \v -> do
    fSymbol :: Int <- v .: "symbol"
    fBalance :: String <- v .: "balance"
    fName :: String <- v .: "name"
    fFrozen :: String <- v .: "frozen"
    return $ BalanceData fSymbol (read fBalance) fName (read fFrozen)

data MarketOrder = MarketOrder {
  price :: Double
  , volume :: Double
  , total :: Double
} deriving (Generic, Show)

instance FromJSON MarketOrder where
  parseJSON = withArray "MarketOrder" $ \v' -> do
    let v = fmap parseJSON v'
    MarketOrder <$> (v ! 0) <*> (v ! 1) <*> (v ! 2)

data MarketDepth = MarketDepth {
  asks :: [MarketOrder]
  , bids :: [MarketOrder]
} deriving (Generic, Show)

instance FromJSON MarketDepth

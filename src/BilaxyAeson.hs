{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BilaxyAeson (
  BilaxyResponse(..),
  BalanceData(..)
)
where

import GHC.Generics
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import           Data.Aeson.Types         ( parseMaybe, parseEither, Result(..) )

import Debug.Trace (trace)

data BilaxyResponse a = BilaxyResponse {
  code :: Int
  , brData :: a} deriving (Show)

data BalanceData = BalanceData {
  symbol :: Int
  , balance :: Integer
  , name :: String
  , frozen :: Integer } deriving (Generic, Show)

instance (FromJSON a) => FromJSON (BilaxyResponse a) where
  parseJSON = withObject "BilaxyResponse" $ \v -> do
    fCode :: String <- v .: "code"
    fData :: a <- v .: "data"
    return $ BilaxyResponse (read fCode) fData

instance FromJSON BalanceData where
  parseJSON = withObject "BalanceData" $ \v -> do
    fSymbol :: Int <- v .: "symbol"
    fBalance :: String <- v .: "balance"
    fName :: String <- v .: "name"
    fFrozen :: String <- v .: "frozen"
    return $ BalanceData fSymbol (read fBalance) fName (read fFrozen)

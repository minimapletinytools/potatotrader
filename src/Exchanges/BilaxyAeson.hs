module Exchanges.BilaxyAeson (
  BilaxyResponse(..),
  BalanceData(..),
  BalanceDataMap(..),
  sortBalanceData,
  MarketOrder(..),
  MarketDepth(..),
  OrderInfo(..)
)
where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map         as M
import           Data.Maybe
import           Data.Time.Clock
import           Data.Vector      ((!))
import           GHC.Generics

import           Debug.Trace      (trace)

data BilaxyResponse a = BilaxyResponse {
  code     :: Int
  , brData :: a
} deriving (Show)

instance (FromJSON a) => FromJSON (BilaxyResponse a) where
  parseJSON = withObject "BilaxyResponse" $ \v -> do
    fCode :: String <- v .: "code"
    fData :: a <- v .: "data"
    return $ BilaxyResponse (read fCode) fData

data BalanceData = BalanceData {
  symbol    :: Int
  , balance :: Double
  , name    :: String
  , frozen  :: Double
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
  price    :: Double
  , volume :: Double
  , total  :: Double
} deriving (Generic, Show)

instance FromJSON MarketOrder where
  parseJSON = withArray "MarketOrder" $ \v' -> do
    let v = fmap parseJSON v'
    MarketOrder <$> (v ! 0) <*> (v ! 1) <*> (v ! 2)

data MarketDepth = MarketDepth {
  asks   :: [MarketOrder]
  , bids :: [MarketOrder]
} deriving (Generic, Show)

instance FromJSON MarketDepth

data OrderStatus = NotTradedYet | TradedPartly | TradedCompletely | Cancelled deriving (Show)
instance FromJSON OrderStatus where
  parseJSON = withScientific "OrderStatus" $ \n -> return $ case n of
    1 -> NotTradedYet
    2 -> TradedPartly
    3 -> TradedCompletely
    4 -> Cancelled
    _ -> error $ "unknown status " ++ show n


data OrderInfo = OrderInfo {
  oi_datetime      :: ()
  , oi_amount      :: Double
  , oi_price       :: Double
  , oi_count       :: Double
  , oi_symbol      :: Int
  , oi_pairId      :: Int
  , oi_left_amount :: Double
  , oi_left_count  :: Double
  , oi_status      :: OrderStatus
} deriving (Generic, Show)

instance FromJSON OrderInfo where
  parseJSON = withObject "OrderInfo" $ \v -> do
    -- TODO fix serialization
    --fDatetime :: UTCTime <- v .: "datetime"
    fAmount :: String <- v .: "amount"
    fPrice :: String <- v .: "price"
    fCount :: String <- v .: "count"
    fId :: Int <- v .: "id"
    fLeft_amount :: String <- v .: "left_amount"
    fLeft_count :: String <- v .: "left_count"
    fStatus :: OrderStatus <- v .: "status"
    fSymbol :: Int <- v .: "symbo" -- typo in API
    return $ OrderInfo {
      --oi_datetime = fDatetime
      oi_datetime = ()
      , oi_amount = read fAmount
      , oi_price = read fPrice
      , oi_count = read fCount
      , oi_symbol = fSymbol
      , oi_pairId = fId
      , oi_left_amount = read fLeft_amount
      , oi_left_count = read fLeft_count
      , oi_status = fStatus
    }

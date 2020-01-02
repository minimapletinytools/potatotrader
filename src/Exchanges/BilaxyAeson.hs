module Exchanges.BilaxyAeson (
  BilaxyResponse(..),
  BalanceData(..),
  BalanceDataMap(..),
  sortBalanceData,
  MarketOrder(..),
  MarketDepth(..),
  OrderInfo(..),
  TradeExecResult(..),
  RateLimit(..)
)
where

import           Control.Monad    (foldM)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map         as M
import           Data.Maybe
import           Data.Text        (Text)
import           Data.Time.Clock
import           Data.Vector      ((!))
import           Debug.Trace      (trace)
import           GHC.Generics

lookupMany :: (FromJSON a) => Object -> [Text] -> Parser a
lookupMany v x = do
  let
    foldfn :: (FromJSON a) => Maybe a -> Text -> Parser (Maybe a)
    foldfn b s = case b of
      Nothing -> v .:? s
      found   -> return found
  found <- foldM foldfn Nothing x
  case found of
    Nothing -> error $ "found none of: " ++ show x
    Just r  -> return r


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
  , oi_id          :: Int
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
    fSymbol :: Int <- lookupMany v ["symbo", "symbol"] -- typo in API
    return $ OrderInfo {
      --oi_datetime = fDatetime
      oi_datetime = ()
      , oi_amount = read fAmount
      , oi_price = read fPrice
      , oi_count = read fCount
      , oi_symbol = fSymbol
      , oi_id = fId
      , oi_left_amount = read fLeft_amount
      , oi_left_count = read fLeft_count
      , oi_status = fStatus
    }

data TradeExecResult = TradeExecResult {
  ter_resultCode :: Int
  , ter_id       :: Int
} deriving (Show)

instance FromJSON TradeExecResult where
  parseJSON = withObject "TradeExecResult" $ \v -> do
    fResultCode :: Int <- v .: "resultCode"
    fId :: Int <- v .: "id"
    return $ TradeExecResult fResultCode fId

-- TODO parse interval into units of time
data RateLimit = RateLimit {
  interval    :: String
  , max_times :: Int
} deriving (Show, Generic)

instance FromJSON RateLimit

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}


module Potato.CryptoTrader.Exchanges.Bilaxy.Query (
  DecodeError(..),

  getTicker,
  getBalanceOf,
  getOrderInfo,
  getOrderList,
  getDepth,
  getRateLimit,
  postOrder,
  cancelOrder,

  recordDepth,

  testDepth,
  testBalance,
  send
)

where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import qualified Crypto.Hash.SHA1                             as SHA1
import           Data.Aeson
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Builder                      as BSB
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.ByteString.UTF8                         as BS
import           Data.Double.Conversion.ByteString
import qualified Data.Map                                     as M
import           Data.Sort                                    (sort)
import           Data.Text.Encoding
import           Data.Time
import           Debug.Trace                                  (trace)
import           Network.HTTP.Simple                          hiding (httpLBS)
import           Potato.CryptoTrader.Exchanges.Bilaxy.Account
import qualified Potato.CryptoTrader.Exchanges.Bilaxy.Aeson   as BA
import qualified Potato.CryptoTrader.Types                    as T
import           System.IO
import           System.IO.Error
import           Text.Printf

-- | 🥔🥔🥔
class Monad m => MonadPotatoDebug m where
  -- this is messing up my bytestring output formatting D:
  consoleDebug :: (Show s) => s -> m ()
  consoleDebug s = trace (show s) $ return ()

instance MonadPotatoDebug IO where
  consoleDebug = print

class Monad m => MonadHttp m where
  httpLBS :: Request -> m (Response LBS.ByteString)

instance MonadHttp IO where
  httpLBS = httpLbs

class Monad m => MonadKeyReader m where
  readKeys :: m BilaxyAccount

instance MonadKeyReader IO where
  readKeys = readBilaxAccount

-- TODO once we add keys
--instance (Monad m) => MonadKeyReader (ReaderT ((),BilaxyAccount) m) where
  --readKeys = snd <$> ask

type MonadQuery m = (Monad m, MonadPotatoDebug m, MonadHttp m, MonadThrow m, MonadKeyReader m)

-- TODO test fixture class
-- see https://lexi-lambda.github.io/blog/2016/10/03/using-types-to-unit-test-in-haskell/
{-
data MonadQueryInst ..
newtype TestM log a =
    TestM (ReaderT (MonadFSInst (TestM log)) (Writer log) a)
  deriving ( Functor, Applicative, Monad
           , MonadReader (MonadFSInst (TestM log))
           , MonadWriter log
           )
-}

toStrict1 :: LBS.ByteString -> BS.ByteString
toStrict1 = BS.concat . LBS.toChunks

type Params = [(BS.ByteString, BS.ByteString)]

newtype DecodeError = DecodeError String
  deriving (Show)
instance Exception DecodeError

-- | signParams takes a BilaxyAccount and query parameters and signs according to Bilaxy's requirements
signParams :: BilaxyAccount -> Params -> Params
signParams (pub, sec) params = final
  where
    params' = params ++ [("key", pub)]
    p = joinParams $ sort (params' ++ [("secret", sec)])
    hash = toStrict1 .BSB.toLazyByteString . BSB.byteStringHex $ SHA1.hash p
    final = params' ++ [("sign", hash)]
    joinParams :: Params -> BS.ByteString
    joinParams = foldl1 (\x y -> x <> "&" <> y) . map (\(x,y)-> x <> "=" <> y)

oldGateway = "https://api.bilaxy.com"
newGateway = "https://newapi.bilaxy.com"

-- | query sets the method, path and params of a Request
query :: BS.ByteString -> BS.ByteString -> Params -> Request -> Request
query method path query =
  setRequestMethod method .
  setRequestPath path .
  setRequestQueryString (toQuery query)
  where
    toQuery = map (\(x,y) -> (x, Just y))

-- | generateRequest generates a open Bilaxy API request with given method, path and params
generateRequest :: MonadThrow m => String -> BS.ByteString -> BS.ByteString -> Params -> m Request
generateRequest gateway method path q = do
  r <- parseRequest gateway
  return $ query method path q r

-- | generatePrivRequest generates a private Bilaxy API request with given BilaxyAccount, method, path, and params
generatePrivRequest :: MonadThrow m => String -> BilaxyAccount -> BS.ByteString -> BS.ByteString -> Params -> m Request
generatePrivRequest gateway kp method path q = do
  let q2 = signParams kp q
  r <- parseRequest gateway
  return $ query method path q2 r

-- | makeRequest makes a request and converts JSON return value into a haskell object
makeRequest :: (MonadQuery m, FromJSON a) => BilaxyAccount -> String -> BS.ByteString -> BS.ByteString -> Params -> m a
makeRequest kp gateway method path params = do
  let priv = kp /= nilKey
  request <- if priv
    then generatePrivRequest gateway kp method path params
    else generateRequest gateway method path params
  response <- httpLBS request
  --consoleDebug $ "querying (priv="++(show priv)++"):\n"++(show request)
  --consoleDebug $ "response status code: " ++ show (getResponseStatusCode response)
  --printResponse response
  let x = eitherDecode $ getResponseBody response :: (FromJSON a) => Either String a
  case x of
    Left v  -> do
      consoleDebug "&&&BILAXY REQUEST FAILED&&&"
      consoleDebug $ "querying (priv="++(show priv)++"):\n"++(show request)
      consoleDebug $ "response status code: " ++ show (getResponseStatusCode response)
      consoleDebug $ "response: "
      printResponse response
      throwM $ DecodeError $ "bilaxy decode error: " ++ show v
    Right a -> return a

-- | makeStandardResponseRequest calls makeRequest assuming the return type is wrapped in BA.BilaxyResponse
makeStandardResponseRequest :: (MonadQuery m, FromJSON a) => BilaxyAccount -> String -> BS.ByteString -> BS.ByteString -> Params -> m a
makeStandardResponseRequest kp gateway method path params = do
  BA.BilaxyResponse code a <- makeRequest kp gateway method path params
  case code of
    200 -> return a
    _   -> error $ "bad return code: " ++ show code

showBS :: (Show a) => a -> BS.ByteString
showBS = BS.fromString . show

-- | getTicker returns price ticker for given pair
getTicker :: Int -> IO BA.Ticker
getTicker pair = do
  makeStandardResponseRequest nilKey oldGateway "GET" "/v1/ticker/" [("symbol", showBS pair)]

-- | pullBalance returns (balance, frozen) of key from a BalanceData query
pullBalance :: BA.BalanceDataMap -> String -> Maybe (Double, Double)
pullBalance bdm key = do
  bd <- M.lookup key bdm
  return (BA.balance bd, BA.frozen bd)

-- | getBalanceOf returns balance of given symbol
getBalanceOf :: String -> IO Double
getBalanceOf symbol = do
  kp <- readKeys
  bd <- makeStandardResponseRequest kp oldGateway "GET" "/v1/balances/" []
  let
    sortedbd = BA.sortBalanceData bd
    maybeBalance = pullBalance sortedbd symbol
  case maybeBalance of
    Nothing -> error $ "could not find " ++ symbol ++ " in " ++ show bd
    Just b  -> return . fst $ b

-- TODO what happens when order does not exist, probably same as cancelOrder
getOrderInfo :: Int -> IO BA.OrderInfo
getOrderInfo orderId = do
  kp <- readKeys
  makeStandardResponseRequest kp oldGateway "GET" "/v1/trade_view" [("id", showBS orderId)]

getOrderList :: Int -> IO [BA.OrderInfo]
getOrderList pair = do
  kp <- readKeys
  makeStandardResponseRequest kp oldGateway "GET" "/v1/trade_list" [("symbol", showBS pair),("type", "1")]

-- | getDepth returns market depth
getDepth :: Int -> IO BA.MarketDepth
getDepth pair = do
  makeStandardResponseRequest nilKey oldGateway "GET" "/v1/depth" [("symbol", showBS pair),("type", "1")]

getRateLimit :: IO BA.RateLimit
getRateLimit = do
  [r] <- makeRequest nilKey newGateway "GET" "/rate_limits" []
  return r

-- amount is in units of first token
-- price is in units of second token
-- e.g. TT/USDT
-- amount is TT to be bought/sold and price is price per TT in USDT
-- NOTE seems like you can't make a Buy order for too low a price or you'll get:
-- {"minAmount":1.0,"resultCode":-3}
postOrder :: Int -> Double -> Double -> T.OrderType -> IO Int
postOrder pair amount price orderType = do
  let
    ot = case orderType of
      T.Buy  -> "buy"
      T.Sell -> "sell"
  kp <- readKeys
  BA.TradeExecResult code oid <- makeRequest kp oldGateway "POST" "/v1/trade"
    -- TODO this is wrong, hardcode to work with TT/USDT pairs right now...
    [("symbol", showBS pair),("amount", toFixed 0 amount),("price", toFixed 5 price),("type", ot)]
  return oid

-- TODO if order id does not exist returns {"code":"401","data":null}
-- which causes parser to throw an error
cancelOrder :: Int -> IO ()
cancelOrder oid = do
  kp <- readKeys
  BA.BilaxyResponse code (rid :: Int) <- makeRequest kp oldGateway "POST" "/v1/cancel_trade" [("id", showBS oid)]
  return ()



-- | helper for generating fake test data
recordDepth :: Int -> Int -> IO ()
recordDepth pair seconds = do
  depth <- replicateM seconds $ do
    r <- getDepth pair
    -- sleep for 1 second
    threadDelay 1000000
    return r
  LBS.writeFile "bilaxy_depth_data.txt" (encode depth)








-- TODO delete stuff below
printResponse :: (MonadPotatoDebug m) => Response LBS.ByteString -> m ()
printResponse response = do
  consoleDebug $ "The status code was: " ++
    show (getResponseStatusCode response)
  consoleDebug $ getResponseHeader "Content-Type" response
  consoleDebug $ getResponseBody response

testBalance :: IO ()
testBalance = do
  b <- getBalanceOf "TT"
  print b

testDepth :: IO ()
testDepth = do
  --depth <- getDepth 151
  --print depth
  getRateLimit >>= print

send :: IO ()
send = do
  print "getting trades"


testOrder :: IO ()
testOrder = do
  print "making order"
  -- sells 1000 TT for 0.008 USDT each
  oid <- postOrder 151 1000 0.008 T.Sell
  -- buys 2 TT for 0.00001 USDT each
  --oid <- postOrder 151 1000 0.0071 Buy
  print oid
  print "order status"
  order <- getOrderInfo oid
  print order
  print "getting orders:"
  x <- getOrderList 151
  print x
  --print "cancel all orders"
  --mapM_ (cancelOrder . BA.oi_id) x
  --testDepth

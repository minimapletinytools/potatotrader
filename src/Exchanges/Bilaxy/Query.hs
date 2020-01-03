{-# LANGUAGE OverloadedStrings #-}


module Exchanges.Bilaxy.Query (
  DecodeError(..),

  getTicker,
  getBalanceOf,
  getOrderInfo,
  getOrderList,
  getDepth,
  getRateLimit,
  postOrder,
  cancelOrder,

  testDepth,
  testBalance,
  send
)

where

import           Control.Exception
import           Control.Monad              (mapM_)
import qualified Crypto.Hash.SHA1           as SHA1
import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BSB
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.UTF8       as BS
import qualified Data.Map                   as M
import           Data.Sort                  (sort)
import           Data.Text.Encoding
import           Data.UnixTime
import           Debug.Trace                (trace)
import qualified Exchanges.Bilaxy.Aeson     as BA
import           Network.HTTP.Simple
import           System.IO
import           System.IO.Error
import           Text.Printf


toStrict1 :: LBS.ByteString -> BS.ByteString
toStrict1 = BS.concat . LBS.toChunks

type Params = [(BS.ByteString, BS.ByteString)]
type KeyPair = (BS.ByteString, BS.ByteString)

newtype DecodeError = DecodeError String
  deriving (Show)
instance Exception DecodeError
instance Eq DecodeError where
  (==) a b = True

-- TODO prompt for password and decrypt
-- | readKeys reads an unencrypted Bilaxy API key pair from file assuming first line is pub key and second line is secret
readKeys :: IO (BS.ByteString, BS.ByteString)
readKeys = do
    handle <- openFile "keys.txt" ReadMode
    pub <- BS.hGetLine handle
    sec <- BS.hGetLine handle
    hClose handle
    return (pub, sec)

-- | signParams takes a KeyPair and query parameters and signs according to Bilaxy's requirements
signParams :: KeyPair -> Params -> Params
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
generateRequest :: String -> BS.ByteString -> BS.ByteString -> Params -> IO Request
generateRequest gateway method path q = do
  r <- parseRequest gateway
  return $ query method path q r

-- | generatePrivRequest generates a private Bilaxy API request with given keypair, method, path, and params
generatePrivRequest :: String -> KeyPair -> BS.ByteString -> BS.ByteString -> Params -> IO Request
generatePrivRequest gateway kp method path q = do
  let q2 = signParams kp q
  r <- parseRequest gateway
  return $ query method path q2 r

-- | makeRequest makes a request and converts JSON return value into a haskell object
makeRequest :: (FromJSON a) => String -> Bool -> BS.ByteString -> BS.ByteString -> Params -> IO a
makeRequest gateway priv method path params = do
  kp <- readKeys
  request <- if priv
    then generatePrivRequest gateway kp method path params
    else generateRequest gateway method path params
  printf "querying (priv=%s):\n%s" (show priv) (show request)
  response <- httpLBS request
  --putStrLn $ "response status code: " ++ show (getResponseStatusCode response)
  printResponse response
  let x = eitherDecode $ getResponseBody response :: (FromJSON a) => Either String a
  case x of
    Left v  -> throwIO $ DecodeError $ "bilaxy decode error: " ++ show v
    Right a -> return a

-- | makeStandardResponseRequest calls makeRequest assuming the return type is wrapped in BA.BilaxyResponse
makeStandardResponseRequest :: (FromJSON a) => String -> Bool -> BS.ByteString -> BS.ByteString -> Params -> IO a
makeStandardResponseRequest gateway priv method path params = do
  BA.BilaxyResponse code a <- makeRequest gateway priv method path params
  case code of
    200 -> return a
    _   -> error $ "bad return code: " ++ show code


showBS :: (Show a) => a -> BS.ByteString
showBS = BS.fromString . show

-- | getTicker returns price ticker for given pair
getTicker :: Int -> IO BA.Ticker
getTicker pair = do
  makeStandardResponseRequest oldGateway True "GET" "/v1/ticker/" [("symbol", showBS pair)]

-- | pullBalance returns (balance, frozen) of key from a BalanceData query
pullBalance :: BA.BalanceDataMap -> String -> Maybe (Double, Double)
pullBalance bdm key = do
  bd <- M.lookup key bdm
  return (BA.balance bd, BA.frozen bd)

-- | getBalanceOf returns balance of given symbol
getBalanceOf :: String -> IO Double
getBalanceOf symbol = do
  bd <- makeStandardResponseRequest oldGateway True "GET" "/v1/balances/" []
  let
    sortedbd = BA.sortBalanceData bd
    maybeBalance = pullBalance sortedbd symbol
  case maybeBalance of
    Nothing -> error $ "could not find " ++ symbol ++ " in " ++ show bd
    Just b  -> return . fst $ b

-- TODO what happens when order does not exist, probably same as cancelOrder
getOrderInfo :: Int -> IO BA.OrderInfo
getOrderInfo orderId = makeStandardResponseRequest oldGateway True "GET" "/v1/trade_view" [("id", showBS orderId)]

getOrderList :: Int -> IO [BA.OrderInfo]
getOrderList pair = makeStandardResponseRequest oldGateway True "GET" "/v1/trade_list" [("symbol", showBS pair),("type", "1")]

-- | getDepth returns market depth
getDepth :: Int -> IO BA.MarketDepth
getDepth pair = makeStandardResponseRequest oldGateway False "GET" "/v1/depth" [("symbol", showBS pair),("type", "1")]

getRateLimit :: IO BA.RateLimit
getRateLimit = do
  [r] <- makeRequest newGateway False "GET" "/rate_limits" []
  return r

data OrderType = Buy | Sell
instance Show OrderType where
  show Buy  = "buy"
  show Sell = "sell"

-- amount is in units of first token
-- price is in units of second token
-- e.g. TT/USDT
-- amount is TT to be bought/sold and price is price per TT in USDT
-- TODO seems like you can't make a Buy order for too low a price or you'll get:
-- {"minAmount":1.0,"resultCode":-3}
-- in any case should try and catch errors and return -1 if order fails
postOrder :: Int -> Double -> Double -> OrderType -> IO Int
postOrder pair amount price orderType = do
  BA.TradeExecResult code oid <- makeRequest oldGateway True "POST" "/v1/trade"
    [("symbol", showBS pair),("amount", showBS amount),("price", showBS price),("type", showBS orderType)]
  return oid

-- TODO if order id does not exist returns {"code":"401","data":null}
-- which causes parser to throw an error
cancelOrder :: Int -> IO ()
cancelOrder oid = do
  BA.BilaxyResponse code (rid :: Int) <- makeRequest oldGateway True "POST" "/v1/cancel_trade" [("id", showBS oid)]
  return ()

-- TODO delete stuff below
printResponse :: Response L8.ByteString -> IO ()
printResponse response = do
  putStrLn $ "The status code was: " ++
    show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  L8.putStrLn $ getResponseBody response


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
  oid <- postOrder 151 1000 0.008 Sell
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

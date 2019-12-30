{-# LANGUAGE OverloadedStrings #-}


module Exchanges.BilaxyHttp (
  getBalanceOf
)
where

import           Arbitrage

import qualified Crypto.Hash.SHA1           as SHA1
import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BSB
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map                   as M
import           Data.Sort                  (sort)
import           Data.Text.Encoding
import           Debug.Trace                (trace)
import qualified Exchanges.BilaxyAeson      as BA
import           Network.HTTP.Simple
import           System.IO

toStrict1 :: LBS.ByteString -> BS.ByteString
toStrict1 = BS.concat . LBS.toChunks

type Params = [(BS.ByteString, BS.ByteString)]
type KeyPair = (BS.ByteString, BS.ByteString)

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

bilaxyTradingPairIds :: [(String, BS.ByteString)]
bilaxyTradingPairIds = [("TT/USDT","151")
                        , ("BIA/USDT","167")
                        , ("ETH/USDT","79")]

bilaxyGateway = "https://api.bilaxy.com"

tickerRequestQuery=[("symbol","151")]
depthRequestQuery=[("symbol","151")]
txRequestQuery=[("symbol","151"),("size","10")]
balanceRequestQuery = []
tradeListRequestQuery=[("since","1572566400"),("symbol","151"),("type","0")]

-- | query sets the method, path and params of a Request
query :: BS.ByteString -> BS.ByteString -> Params -> Request -> Request
query method path query =
  setRequestMethod method .
  setRequestPath path .
  setRequestQueryString (toQuery query)
  where
    toQuery = map (\(x,y) -> (x, Just y))

-- | generateRequest generates a open Bilaxy API request with given method, path and params
generateRequest :: BS.ByteString -> BS.ByteString -> Params -> IO Request
generateRequest method path q = do
  r <- parseRequest bilaxyGateway
  return $ query method path q r

-- | generatePrivRequest generates a private Bilaxy API request with given keypair, method, path, and params
generatePrivRequest :: KeyPair -> BS.ByteString -> BS.ByteString -> Params -> IO Request
generatePrivRequest kp method path q = do
  let q2 = signParams kp q
  r <- parseRequest bilaxyGateway
  return $ query method path q2 r

-- tickerRequest makes a ticker request for the given pair
tickerRequest :: IO Request
tickerRequest = generateRequest "GET" "/v1/ticker/" tickerRequestQuery

depthRequest :: IO Request
depthRequest = generateRequest "GET" "/v1/depth/" depthRequestQuery

txRequest :: IO Request
txRequest = generateRequest "GET" "/v1/orders/" txRequestQuery

tradeListRequest :: KeyPair -> IO Request
tradeListRequest kp = generatePrivRequest kp "GET" "/v1/trade_list/" tradeListRequestQuery

balanceRequest :: KeyPair -> IO Request
balanceRequest kp = generatePrivRequest kp "GET" "/v1/balances/" balanceRequestQuery

-- pullBalance returns (balance, frozen) of key from a BalanceData query
pullBalance :: BA.BalanceDataMap -> String -> Maybe (Double, Double)
pullBalance bdm key = do
  bd <- M.lookup key bdm
  return (BA.balance bd, BA.frozen bd)

-- TODO need to convert units
--getLiquidityPair :: (String, String) -> BA.BalanceDataMap -> Liquidity
--getLiquidityPair keys bdm = fmap (fst . pullBalance $ bdm) keys

getBalanceOf :: String -> IO Double
getBalanceOf symbol = do
  kp <- readKeys
  request <- balanceRequest kp
  putStrLn $ "querying: " ++ show request
  response <- httpLBS request
  let x = eitherDecode $ getResponseBody response :: Either String (BA.BilaxyResponse [BA.BalanceData])
  case x of
    Left v -> error $ "bilaxy query error: " ++ show v
    Right (BA.BilaxyResponse _ bd) -> ret where
      sortedbd = BA.sortBalanceData bd
      maybeBalance = pullBalance sortedbd symbol
      ret = case maybeBalance of
        Nothing -> error $ "could not find " ++ symbol ++ " in " ++ show bd
        Just b  -> return . fst $ b








-- TODO delete stuff below
printResponse :: Response L8.ByteString -> IO ()
printResponse response = do
  putStrLn $ "The status code was: " ++
    show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  L8.putStrLn $ getResponseBody response


testBalance :: IO ()
testBalance = do
  kp <- readKeys
  request <- balanceRequest kp
  print request
  response <- httpLBS request
  let x = eitherDecode $ getResponseBody response :: Either String (BA.BilaxyResponse [BA.BalanceData])
  case x of
    Left v -> print v
    Right (BA.BilaxyResponse _ bd) -> print $ pullBalance (BA.sortBalanceData bd) "TT"

testDepth :: IO ()
testDepth = do
  request <- depthRequest
  print request
  response <- httpLBS request
  printResponse response
  let x = eitherDecode $ getResponseBody response :: Either String (BA.BilaxyResponse BA.MarketDepth)
  case x of
    Left v                         -> print v
    Right (BA.BilaxyResponse _ md) -> print md

send :: IO ()
send = do
  testDepth
  --kp <- readKeys
  --request <- tickerRequest
  --request <- txRequest
  --request <- tradeListRequest kp
  --print request
  --response <- httpLBS request
  --printResponse response

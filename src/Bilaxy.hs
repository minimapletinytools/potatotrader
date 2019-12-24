{-# LANGUAGE OverloadedStrings #-}

module Bilaxy (
  send
)
where

import Data.Sort (sort)
import Data.Text.Encoding
import System.IO
import Data.Aeson
import Network.HTTP.Simple
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as L8
import Debug.Trace (trace)

toStrict1 :: LBS.ByteString -> BS.ByteString
toStrict1 = BS.concat . LBS.toChunks

type Params = [(BS.ByteString, BS.ByteString)]
type KeyPair = (BS.ByteString, BS.ByteString)

readKeys :: IO (BS.ByteString, BS.ByteString)
readKeys = do
    handle <- openFile "keys.txt" ReadMode
    pub <- BS.hGetLine handle
    sec <- BS.hGetLine handle
    hClose handle
    return (pub, sec)

joinParams :: Params -> BS.ByteString
joinParams = foldl1 (\x y -> x <> "&" <> y) . map (\(x,y)-> x <> "=" <> y)

signParams :: KeyPair -> Params -> Params
signParams (pub, sec) params = final
  where
    params' = params ++ [("key", pub)]
    p = joinParams $ sort (params' ++ [("secret", sec)])
    hash = toStrict1 .BSB.toLazyByteString . BSB.byteStringHex $ SHA1.hash p
    final = params' ++ [("sign", hash)]


--tickerRequestQuery ="{\"symbol=151\"}"
tickerRequestQuery=[("symbol","151")]
depthRequestQuery=[("symbol","151")]
txRequestQuery=[("symbol","151"),("size","10")]
balanceRequestQuery = []
tradeListRequestQuery=[("since","1572566400"),("symbol","151"),("type","0")]


toQuery :: Params -> Query
toQuery = map (\(x,y) -> (x, Just y))

query :: BS.ByteString -> BS.ByteString -> Params -> Request -> Request
query method path query =
  setRequestMethod method .
  setRequestPath path .
  setRequestQueryString (toQuery query)

generateRequest :: BS.ByteString -> BS.ByteString -> Params -> IO Request
generateRequest method path q = do
  r <- parseRequest "https://api.bilaxy.com"
  return $ query method path q r

generatePrivRequest :: KeyPair -> BS.ByteString -> BS.ByteString -> Params -> IO Request
generatePrivRequest kp method path q = do
  let q2 = signParams kp q
  r <- parseRequest "https://api.bilaxy.com"
  return $ query method path q2 r

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

printResponse :: Response L8.ByteString -> IO ()
printResponse response = do
  putStrLn $ "The status code was: " ++
    show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  L8.putStrLn $ getResponseBody response

send :: IO ()
send = do
  kp <- readKeys
  --request <- depthRequest
  --request <- tickerRequest
  --request <- txRequest
  request <- tradeListRequest kp
  --request <- balanceRequest kp
  print request
  response <- httpLBS request
  printResponse response

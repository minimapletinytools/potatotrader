
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

module Eth (
  getPrice,
  testmain,
  paramsToString,
  testTransaction
) where

import System.IO
import Control.Monad (mapM_)
import Data.Function ((&))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteArray.HexString as BS
import qualified Data.Solidity.Prim.Address as WEB3
import qualified Network.Ethereum.Contract.TH as WEB3
import qualified Crypto.Ethereum as WEB3
import qualified Network.Ethereum.Web3 as WEB3
import qualified Network.Ethereum.Account as WEB3
import Lens.Micro ((.~))
import qualified Network.Ethereum.Account.Internal as WEB3
import qualified Network.Ethereum.Api.Provider as WEB3
import qualified Network.Ethereum.Api.Types as WEB3 hiding (blockNumber)
import qualified Network.Ethereum.Api.Eth as WEB3
import qualified Network.JsonRpc.TinyClient as WEB3

import Debug.Trace (trace)

-- load abi
[WEB3.abiFrom|uniswap.json|]


-- # @return Amount of ETH bought.
-- def tokenToEthSwapInput(tokens_sold: uint256, min_eth: uint256(wei), deadline: timestamp) -> uint256(wei):

-- # @return Amount of Tokens that can be bought with input ETH.
-- def getEthToTokenInputPrice(eth_sold: uint256(wei)) -> uint256:

-- # @return Amount of ETH needed to buy output Tokens.
-- def getEthToTokenOutputPrice(tokens_bought: uint256) -> uint256(wei):

-- # @return Amount of ETH that can be bought with input Tokens.
-- def getTokenToEthInputPrice(tokens_sold: uint256) -> uint256(wei):

-- # @return Amount of Tokens needed to buy output ETH.
-- def getTokenToEthOutputPrice(eth_bought: uint256(wei)) -> uint256:


paramsToString :: (Show p) => WEB3.CallParam p -> String
paramsToString p =
  "to: " ++ show (WEB3._to p)
   ++ "\nvalue: " ++ show (WEB3._value p)
   ++ "\ngas limit: " ++ show (WEB3._gasLimit p)
   ++ "\ngas price: " ++ show (WEB3._gasPrice p)
   ++ "\nblock: " ++ show (WEB3._block p)
   ++ "\naccount: " ++ show (WEB3._account p)

readKeys :: Integer -> IO WEB3.LocalKey
readKeys chainId = do
    handle <- openFile "keys.txt" ReadMode
    -- skip bilaxy keys
    _ <- BS.hGetLine handle
    _ <- BS.hGetLine handle
    -- pub key
    _ <- BS.hGetLine handle
    sec <- BS.hGetLine handle
    hClose handle
    let
      Right s = BS.hexString sec
      bs = BS.toBytes s :: BS.ByteString
    --trace (show s) $ return ()
    --trace (show bs) $ return ()
    return $ WEB3.LocalKey (WEB3.importKey (BS.toBytes s :: BS.ByteString)) chainId

{-
compileErrorGiveMeType :: ()
compileErrorGiveMeType = ethToTokenSwapInput

compileErrorGiveMeType2 :: ()
compileErrorGiveMeType2 = getTokenToEthOutputPrice
-}

providerURL = "https://mainnet-rpc.thundercore.com"
--providerURL = "https://mainnet.infura.io/v3/2edbdd953f714eeab3f0001bb0b96b91"
chainID = 108
myAddress = "0xC82bDc455bF1EB9A5e4b9D54979232e2b7340643" :: BS.ByteString

ttUSDT = "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"
uniTTUSDT = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"
uniswapLib = "0x7c1e1a39703ab5c21b5537d4093ccd1117ded405"
uniswapFact = "0xcE393b11872EE5020828e443f6Ec9DE58CD8b6c5"

traceParams :: (Show p,  WEB3.Account p (WEB3.AccountT p)) => WEB3.AccountT p m a -> WEB3.AccountT p m a
traceParams = WEB3.withParam (\x -> trace (paramsToString x) $ x)

type UniFee = (Integer, Integer)
noFee :: UniFee
noFee = (1,1)

-- | calcInputPrice is just uniswap getInputPrice
-- input_amonut is amount being sold, returns amount bought
calcInputPrice :: UniFee -> Integer -> Integer -> Integer -> Integer
calcInputPrice (fn, fd) input_amount input_reserve output_reserve = numerator `div` denominator where
  input_amount_with_fee = input_amount * fn
  numerator = input_amount_with_fee * output_reserve
  denominator = (input_reserve * fd) + input_amount_with_fee

-- | calcFee returns the tx fee fee in output tokens
calcFee :: UniFee -> Integer -> Integer -> Integer -> Integer
calcFee fee input_amount input_reserve output_reserve =
  calcInputPrice noFee input_amount input_reserve output_reserve
  - fee noFee input_amount input_reserve output_reserve

-- | noMarginalGain returns the amount of input tokens that should be bought
-- such that marginal gain given to arbPrice exchange rate is zero
--noMarginalGain :: Double -> UniFee -> Integer -> Integer -> Integer
--noMarginalGain


--solvePrice ::

-- | getUniEthTokenBalances is an operation that takes an exchange address and returns (eth, token) balances
getUniEthTokenBalances :: WEB3.Address -> WEB3.Web3 (Integer, Integer)
getUniEthTokenBalances addr = do
  bn <- WEB3.blockNumber >>= return . WEB3.BlockWithNumber
  balance <- WEB3.getBalance addr bn
  WEB3.withAccount () . WEB3.withParam (WEB3.block .~ bn) $ do
    tAddr <- WEB3.withParam (WEB3.to .~ addr) $ do
      tokenAddress
    tBalance <- WEB3.withParam (WEB3.to .~ tAddr) $ do
      balanceOf addr
    return (toInteger tBalance, toInteger balance)

-- | ethToTokenSwap returns a WEB3 operation to swap tokens at a given exchange
-- calls the following function:
-- # @return Amount of Tokens bought.
-- def ethToTokenSwapInput(min_tokens: uint256, deadline: timestamp) -> uint256:
ethToTokenSwap :: WEB3.LocalKey -> WEB3.Address -> Integer -> WEB3.Web3 WEB3.TxReceipt
ethToTokenSwap account addr amount = do
  bn <- WEB3.blockNumber
  block <- WEB3.getBlockByNumber bn
  let time = toInteger $ WEB3.blockTimestamp block
  -- this trick is needed to solve for scoping issue on time variable
  time & (\time -> (WEB3.withAccount account
    . WEB3.withParam (WEB3.to .~ addr)
    . WEB3.withParam (WEB3.value .~ (1000 :: WEB3.Ether)) $ do
      ethToTokenSwapInput (fromInteger amount) (fromInteger ((time) + 2000))))

prettyShowResult :: (a -> String) -> Either WEB3.Web3Error a -> String
prettyShowResult _ (Left e) = "error: " ++ show e
prettyShowResult f (Right x) = f x

testTransaction :: IO ()
testTransaction = do
  account <- readKeys chainID
  res <- WEB3.runWeb3' (WEB3.HttpProvider providerURL) $ do
    (tokens, eth) <- getUniEthTokenBalances uniTTUSDT
    return (tokens, eth)
    --receipt <- ethToTokenSwap account uniTTUSDT 1
    --return (receipt, tokens, eth)
  putStrLn $ prettyShowResult show res

getPrice :: IO ()
getPrice = do
  account <- readKeys chainID
  --let
  --  key = "0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20" :: BS.HexString
  --  local = WEB3.LocalKey (WEB3.importKey key) 108
  --  localAddress = WEB3.fromPubKey (WEB3.derivePubKey $ WEB3.importKey key)

  let
    Right addr' = BS.hexString myAddress
    Right addr = WEB3.fromHexString addr'

  --print account
  ret <- WEB3.runWeb3' (WEB3.HttpProvider providerURL) $ do
    -- let withAccount = WEB3.withAccount :: (WEB3.JsonRpc m) => () -> WEB3.DefaultAccount m b -> m b
    WEB3.withAccount () $ do
      msg1 <- WEB3.withParam (WEB3.to .~ uniTTUSDT) $ do
        e <- getEthToTokenOutputPrice 1000049
        return $ show $ (WEB3.fromWei e :: WEB3.Ether)
      msg2 <- WEB3.withParam (WEB3.to .~ ttUSDT) $ do
        e <- balanceOf addr
        return $ show e
      return $ [msg1,msg2]
    {-
    let withAccount = WEB3.withAccount :: (WEB3.JsonRpc m) => WEB3.LocalKey -> WEB3.LocalKeyAccount m b -> m b
    withAccount account $ do
      WEB3.withParam (WEB3.to .~ uniTTUSDT)
         $ WEB3.withParam (\x -> trace (paramsToString x) $ x)
        $ tokenAddress
      --getTokenToEthOutputPrice 100
    -}


    -- get balance
    --let
    --  Right addr' = BS.hexString myAddress
    --  Right addr = WEB3.fromHexString addr'
    --WEB3.getBalance addr WEB3.Latest


  print "hi"
  case ret of
    Left e  -> error $ show e
    Right v -> mapM_ print v

testmain :: IO ()
testmain = do
  -- Use default provider on http://localhost:8545
  ret <- WEB3.runWeb3' (WEB3.HttpProvider providerURL) $ do
    let
      Right addr' = BS.hexString myAddress
      Right me = WEB3.fromHexString addr'

    myBalance <- WEB3.getBalance me WEB3.Latest

    -- Get half of balance
    let halfBalance = WEB3.fromWei (myBalance)

    -- Use default account
    WEB3.withAccount () $ do
      WEB3.withParam (WEB3.value .~ halfBalance) $ do
        WEB3.withParam (WEB3.to .~ me) $ WEB3.send ()
        WEB3.withParam (WEB3.to .~ me) $ WEB3.send ()

    -- Return sended value
    return halfBalance
  -- Web3 error handling
  case ret of
      Left e  -> error $ show e
      Right v -> print (v :: WEB3.Ether)  -- Print returned value in ethers


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

module Exchanges.Chain.Query (
  getAddress,
  getTokenBalance,
  getBalance,
  txEthToTokenSwap,
  getTransactionByHash,



  getPrice,
  testmain,
  paramsToString,
  testTransaction
) where

import           Types                             hiding (chainId, getBalance)

import           Control.Exception
import           Control.Monad                     (mapM_)
import           Crypto.Ethereum
import qualified Data.ByteArray.HexString          as BS
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Builder           as BS
import           Data.Function                     ((&))
import           Data.Solidity.Prim.Address
import           Lens.Micro                        ((.~))
import           Network.Ethereum.Account
import           Network.Ethereum.Account.Internal
import qualified Network.Ethereum.Api.Eth          as WEB3
import           Network.Ethereum.Api.Provider
import           Network.Ethereum.Api.Types        hiding (blockNumber)
import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Web3
import           Network.JsonRpc.TinyClient
import           System.IO

import           Debug.Trace                       (trace)

-- load abi, uniswap.json includes ERC20 abi
[abiFrom|uniswap.json|]


{-
compileErrorGiveMeType :: ()
compileErrorGiveMeType = ethToTokenSwapInput

compileErrorGiveMeType2 :: ()
compileErrorGiveMeType2 = getTokenToEthOutputPrice
-}

newtype MissingError = MissingError String
  deriving (Show)
instance Exception MissingError

paramsToString :: (Show p) => CallParam p -> String
paramsToString p =
  "to: " ++ show (_to p)
   ++ "\nvalue: " ++ show (_value p)
   ++ "\ngas limit: " ++ show (_gasLimit p)
   ++ "\ngas price: " ++ show (_gasPrice p)
   ++ "\nblock: " ++ show (_block p)
   ++ "\naccount: " ++ show (_account p)

readKeys :: Integer -> IO LocalKey
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
    return $ LocalKey (importKey (BS.toBytes s :: BS.ByteString)) chainId

getKeyPair :: Integer -> IO (LocalKey, Address)
getKeyPair cid = do
  k@(LocalKey pk _) <- readKeys cid
  return (k, fromPubKey . derivePubKey $ pk)

doweb3stuff :: String ->  Web3 a -> IO a
doweb3stuff url op = do
  ret <- runWeb3' (HttpProvider url) op
  case ret of
    Left e  -> throwIO $ e
    Right v -> return v

-- | getAddress returns the public address of the signing key on disk
getAddress :: IO Address
getAddress = getKeyPair 1 >>= return . snd

-- | getBalance
getBalance :: String -> IO Integer
getBalance url = do
  addr <- getAddress
  let op = WEB3.getBalance addr Latest
  r <- doweb3stuff url op
  return $ toInteger r

-- | getTokenBalance
getTokenBalance :: String -> Address -> IO Integer
getTokenBalance url tAddr = do
  addr <- getAddress
  let op = withAccount () $ withParam (to .~ tAddr) $ balanceOf addr
  r <- doweb3stuff url op
  return $ toInteger r

getTransactionByHash :: String -> BS.HexString -> IO Transaction
getTransactionByHash url hash = do
  r <- doweb3stuff url $ WEB3.getTransactionByHash hash
  case r of
    Nothing -> throwIO $ MissingError "tx not found"
    Just tx -> return tx

txEthToTokenSwap ::
  String
  -> Integer
  -> Address
  -> Wei
  -> Integer
  -> IO TxReceipt
txEthToTokenSwap url cid uniAddr amountEth minTokens = do
  (account,_) <- getKeyPair cid
  blockTime <- doweb3stuff url $ do
    bn <- WEB3.blockNumber
    block <- WEB3.getBlockByNumber bn
    return $ toInteger $ blockTimestamp block
  doweb3stuff url $
    withAccount account
    . withParam (to .~ uniAddr)
    . withParam (value .~ amountEth) $
      ethToTokenSwapInput (fromInteger minTokens) (fromInteger (blockTime + 100))




providerURL = "https://mainnet-rpc.thundercore.com"
--providerURL = "https://mainnet.infura.io/v3/2edbdd953f714eeab3f0001bb0b96b91"
chainId = 108
myAddress = "0xC82bDc455bF1EB9A5e4b9D54979232e2b7340643" :: BS.ByteString

ttUSDT = "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"
uniTTUSDT = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"
uniswapLib = "0x7c1e1a39703ab5c21b5537d4093ccd1117ded405"
uniswapFact = "0xcE393b11872EE5020828e443f6Ec9DE58CD8b6c5"

traceParams :: (Show p,  Account p (AccountT p)) => AccountT p m a -> AccountT p m a
traceParams = withParam (\x -> trace (paramsToString x) $ x)

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
  - calcInputPrice fee input_amount input_reserve output_reserve

-- | noMarginalGain returns the amount of input tokens that should be bought
-- such that marginal gain given to arbPrice exchange rate is zero
--noMarginalGain :: Double -> UniFee -> Integer -> Integer -> Integer
--noMarginalGain


--solvePrice ::

-- | getUniEthTokenBalances is an operation that takes an exchange address and returns (eth, token) balances
getUniEthTokenBalances :: Address -> Web3 (Liquidity TT USDT)
getUniEthTokenBalances addr = do
  tAddr <- withAccount () $ do
    withParam (to .~ addr) tokenAddress
  getLiquidity addr tAddr

-- | ethToTokenSwap returns a WEB3 operation to swap tokens at a given exchange
-- calls the following function:
-- # @return Amount of Tokens bought.
-- def ethToTokenSwapInput(min_tokens: uint256, deadline: timestamp) -> uint256:
ethToTokenSwap :: LocalKey -> Address -> Integer -> Web3 TxReceipt
ethToTokenSwap account addr amount = do
  bn <- WEB3.blockNumber
  block <- WEB3.getBlockByNumber bn
  let time = toInteger $ blockTimestamp block
  -- this trick is needed to solve for scoping issue on time variable
  time & (\time -> withAccount account
    . withParam (to .~ addr)
    . withParam (value .~ (1000 :: Ether)) $ do
      ethToTokenSwapInput (fromInteger amount) (fromInteger (time + 2000)))

-- | getLiquidity returns (TT, token) liquidity in account
getLiquidity :: Address -> Address -> Web3 (Liquidity TT USDT)
getLiquidity addr tAddr = do
  bn <- BlockWithNumber <$> WEB3.blockNumber
  balance <- WEB3.getBalance addr bn
  withAccount () . withParam (block .~ bn) $ do
    tBalance <- withParam (to .~ tAddr) $ do
      balanceOf addr
    return $ Liquidity (Amount $ toInteger balance) (Amount $ toInteger tBalance)

getTTUSDTLiquidity :: Address -> Web3 (Liquidity TT USDT)
getTTUSDTLiquidity addr = getLiquidity addr ttUSDT

prettyShowResult :: (a -> String) -> Either Web3Error a -> String
prettyShowResult _ (Left e)  = "error: " ++ show e
prettyShowResult f (Right x) = f x

testTransaction :: IO ()
testTransaction = do
  account <- readKeys chainId
  res <- runWeb3' (HttpProvider providerURL) $ do
    Liquidity tokens eth <- getUniEthTokenBalances uniTTUSDT
    return (tokens, eth)
    --receipt <- ethToTokenSwap account uniTTUSDT 1
    --return (receipt, tokens, eth)
  putStrLn $ prettyShowResult show res

getPrice :: IO ()
getPrice = do
  account <- readKeys chainId
  --let
  --  key = "0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20" :: BS.HexString
  --  local = LocalKey (importKey key) 108
  --  localAddress = fromPubKey (derivePubKey $ importKey key)

  let
    Right addr' = BS.hexString myAddress
    Right addr = fromHexString addr'

  --print account
  ret <- runWeb3' (HttpProvider providerURL) $ do
    -- let withAccount = withAccount :: (JsonRpc m) => () -> DefaultAccount m b -> m b
    withAccount () $ do
      msg1 <- withParam (to .~ uniTTUSDT) $ do
        e <- getEthToTokenOutputPrice 1000049
        return $ show (fromWei e :: Ether)
      msg2 <- withParam (to .~ ttUSDT) $ do
        e <- balanceOf addr
        return $ show e
      return [msg1,msg2]
    {-
    let withAccount = withAccount :: (JsonRpc m) => LocalKey -> LocalKeyAccount m b -> m b
    withAccount account $ do
      withParam (to .~ uniTTUSDT)
         $ withParam (\x -> trace (paramsToString x) $ x)
        $ tokenAddress
      --getTokenToEthOutputPrice 100
    -}


    -- get balance
    --let
    --  Right addr' = BS.hexString myAddress
    --  Right addr = fromHexString addr'
    --getBalance addr Latest


  print "hi"
  case ret of
    Left e  -> error $ show e
    Right v -> mapM_ print v

testmain :: IO ()
testmain = do
  -- Use default provider on http://localhost:8545
  ret <- runWeb3' (HttpProvider providerURL) $ do
    let
      Right addr' = BS.hexString myAddress
      Right me = fromHexString addr'

    myBalance <- WEB3.getBalance me Latest

    -- Get half of balance
    let halfBalance = fromWei myBalance

    -- Use default account
    withAccount () $ do
      withParam (value .~ halfBalance) $ do
        withParam (to .~ me) $ send ()
        withParam (to .~ me) $ send ()

    -- Return sended value
    return halfBalance
  -- Web3 error handling
  case ret of
      Left e  -> error $ show e
      Right v -> print (v :: Ether)  -- Print returned value in ethers


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

-- warnings disabled for web3 TH generated code
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Exchanges.Chain.Query (
  getAddress,
  getTokenBalance,
  getBalance,
  txEthToTokenSwap,
  getTransactionByHash
) where

import           Control.Exception
import           Control.Monad                     (mapM_)
import           Crypto.Ethereum
import qualified Data.ByteArray.HexString          as BS
import qualified Data.ByteString                   as BS
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

newtype SomeError = SomeError String
  deriving (Show)
instance Exception SomeError
newtype MissingError = MissingError String
  deriving (Show)
instance Exception MissingError
newtype DecodeError = DecodeError String
  deriving (Show)
instance Exception DecodeError

paramsToString :: (Show p) => CallParam p -> String
paramsToString p =
  "to: " ++ show (_to p)
   ++ "\nvalue: " ++ show (_value p)
   ++ "\ngas limit: " ++ show (_gasLimit p)
   ++ "\ngas price: " ++ show (_gasPrice p)
   ++ "\nblock: " ++ show (_block p)
   ++ "\naccount: " ++ show (_account p)

readKeys :: Integer -> IO LocalKey
readKeys cid = do
    h <- openFile "keys.txt" ReadMode
    -- skip bilaxy keys
    _ <- BS.hGetLine h
    _ <- BS.hGetLine h
    -- pub key
    _ <- BS.hGetLine h
    sec <- BS.hGetLine h
    hClose h
    case BS.hexString sec of
      Right s -> return $ LocalKey (importKey (BS.toBytes s :: BS.ByteString)) cid
      Left _ -> throwIO $ DecodeError "invalid secret key"

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
  (acct,_) <- getKeyPair cid
  blockTime <- doweb3stuff url $ do
    bn <- WEB3.blockNumber
    bl <- WEB3.getBlockByNumber bn
    return $ toInteger $ blockTimestamp bl
  doweb3stuff url $
    withAccount acct
    . withParam (to .~ uniAddr)
    . withParam (value .~ amountEth) $
      ethToTokenSwapInput (fromInteger minTokens) (fromInteger (blockTime + 100))









-- TODO delete everything below


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


prettyShowResult :: (a -> String) -> Either Web3Error a -> String
prettyShowResult _ (Left e)  = "error: " ++ show e
prettyShowResult f (Right x) = f x

testTransaction :: IO ()
testTransaction = do
  account <- readKeys chainId
  res <- runWeb3' (HttpProvider providerURL) $ do
    return ()
    --receipt <- ethToTokenSwap account uniTTUSDT 1
    --return (receipt, tokens, eth)
  return ()

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

{-
just an example for testing...
you can delete this...
note that uniswap.json has the full ERC20 ABI in it.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module ERC20 (
  testmain
) where

-- import Eth (paramsToString)
import           Network.Ethereum.Contract.TH
import           Data.Text                (unpack)
import           Text.Printf              (printf)

import           Lens.Micro               ((.~))
import           Network.Ethereum.Account
import           Network.Ethereum.Web3    hiding (name)
import Network.Ethereum.Api.Provider
import Debug.Trace (trace)
--[abiFrom|ERC20.json|]
[abiFrom|uniswap.json|]

providerURL = "https://mainnet-rpc.thundercore.com"
--providerURL = "https://mainnet.infura.io/v3/2edbdd953f714eeab3f0001bb0b96b91"

ttUSDT = "0x4f3C8E20942461e2c3Bdd8311AC57B0c222f2b82"
uniTTUSDT = "0x3e9Ada9F40cD4B5A803cf764EcE1b4Dae6486204"
uniETHMANA = "0xC6581Ce3A005e2801c1e0903281BBd318eC5B5C2"

testmain :: IO ()
testmain = do
    result <- runWeb3' (HttpProvider providerURL) $
        withAccount () $
            withParam (to .~ uniTTUSDT) $ do
                addr <- tokenAddress
                return $ (show addr)
                --n <- name
                --s <- symbol
                --d <- decimals
                --return $ printf "Token %s with symbol %s and decimals %d"
                --   (unpack n) (unpack s) (fromIntegral d :: Int)
    case result of
      Left err   -> error (show err)
      Right info -> putStrLn info

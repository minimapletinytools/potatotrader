module Potato.Trader.Exchanges.Bilaxy.Account (
  BilaxyAccount,
  nilKey,
  readBilaxAccount
) where

import qualified Data.ByteString as BS
import           System.IO

type BilaxyAccount = (BS.ByteString, BS.ByteString)

nilKey :: BilaxyAccount
nilKey = ("","")

-- TODO prompt for password and decrypt :)
-- | readKeys reads an unencrypted Bilaxy API key pair from file assuming first line is pub key and second line is secret
readBilaxAccount :: IO BilaxyAccount
readBilaxAccount = do
    handle <- openFile "keys.txt" ReadMode
    pub <- BS.hGetLine handle
    sec <- BS.hGetLine handle
    hClose handle
    return (pub, sec)

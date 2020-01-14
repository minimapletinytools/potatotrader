module Potato.CryptoTrader.Exchanges.Bilaxy.Account (
  BilaxyAccount(..),
  nilKey
) where

import qualified Data.ByteString as BS


type BilaxyAccount = (BS.ByteString, BS.ByteString)

nilKey :: BilaxyAccount
nilKey = ("","")

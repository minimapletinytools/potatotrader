module Main where

import           Potato.CryptoTrader
import qualified Potato.CryptoTrader.Exchanges.Bilaxy
import qualified Potato.CryptoTrader.Exchanges.Chain

main :: IO ()
main = Potato.CryptoTrader.Exchanges.Bilaxy.recordDepth 151 10
--main = Potato.CryptoTrader.Exchanges.Bilaxy.testBalance
--main = Potato.CryptoTrader.Exchanges.Bilaxy.testDepth
--main = Potato.CryptoTrader.Exchanges.Chain.testTransaction

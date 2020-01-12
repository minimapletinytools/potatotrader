module Main where

import qualified Potato.CryptoTrader.Exchanges.Bilaxy.Query
import qualified Potato.CryptoTrader.Exchanges.Chain.Query

main :: IO ()
--main = Potato.CryptoTrader.Exchanges.Bilaxy.Query.testBalance
main = Potato.CryptoTrader.Exchanges.Bilaxy.Query.testDepth
--main = Potato.CryptoTrader.Exchanges.Chain.Query.testTransaction

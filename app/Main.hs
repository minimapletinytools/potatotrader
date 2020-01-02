module Main where

import qualified Exchanges.Bilaxy.Query
import qualified Exchanges.Chain.Query

main :: IO ()
--main = Exchanges.Bilaxy.Query.testBalance
main = Exchanges.Bilaxy.Query.testDepth
--main = Exchanges.Chain.Query.testTransaction

module Main where

import qualified ERC20
import qualified Eth
import qualified Exchanges.Bilaxy.Query

main :: IO ()
--main = getPrice
--main = ERC20.testmain
--main = sendPriv
--main = Exchanges.Bilaxy.Query.testBalance
main = Exchanges.Bilaxy.Query.testDepth
--main = Exchanges.BilaxyHttp.send
--main = Eth.testTransaction

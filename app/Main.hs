module Main where

import qualified ERC20
import qualified Eth
import qualified Exchanges.BilaxyHttp

main :: IO ()
--main = getPrice
--main = ERC20.testmain
--main = sendPriv
main = Exchanges.BilaxyHttp.testBalance
--main = Eth.testTransaction

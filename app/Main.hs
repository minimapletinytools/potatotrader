module Main where

import qualified ERC20
import qualified Eth
import qualified Exchanges.Bilaxy

main :: IO ()
--main = getPrice
main = ERC20.testmain
--main = sendPriv
--main = Exchanges.Bilaxy.send
--main = Eth.testTransaction

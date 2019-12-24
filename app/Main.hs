module Main where

import qualified Bilaxy
import Eth
import qualified ERC20

main :: IO ()
--main = getPrice
--main = ERC20.testmain
--main = sendPriv
main = Bilaxy.send

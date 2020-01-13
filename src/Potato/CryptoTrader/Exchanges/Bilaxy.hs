{-|
Module      : Potato.CryptoTrader.Exchanges.Bilaxy
Description :
Copyright   : (c) pdlla, 2020
License     : GPL-3
Maintainer  : chippermonky@gmail.com
Stability   : experimental
-}

module Potato.CryptoTrader.Exchanges.Bilaxy (
  module Potato.CryptoTrader.Exchanges.Bilaxy.Exchange,

  -- temp stuff exported for testing
  module Potato.CryptoTrader.Exchanges.Bilaxy.Query
) where

import           Potato.CryptoTrader.Exchanges.Bilaxy.Exchange

-- temp stuff for testing
import           Potato.CryptoTrader.Exchanges.Bilaxy.Query    (testDepth)

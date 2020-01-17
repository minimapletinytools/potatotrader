{-|
Module      : Potato.Trader.Exchanges.Bilaxy
Description :
Copyright   : (c) pdlla, 2020
License     : GPL-3
Maintainer  : chippermonky@gmail.com
Stability   : experimental
-}

module Potato.Trader.Exchanges.Bilaxy (
  module Potato.Trader.Exchanges.Bilaxy.Exchange,
  module Potato.Trader.Exchanges.Bilaxy.Account,

  -- temp stuff exported for testing
  module Potato.Trader.Exchanges.Bilaxy.Query
) where

import           Potato.Trader.Exchanges.Bilaxy.Account
import           Potato.Trader.Exchanges.Bilaxy.Exchange

-- export some utility and testing methods
import           Potato.Trader.Exchanges.Bilaxy.Query    (recordDepth,
                                                                testDepth)

--import           Potato.Trader

import qualified Potato.Trader.Arbitrage.Tests
import qualified Potato.Trader.Exchanges.Bilaxy.Tests
import qualified Potato.Trader.Exchanges.Chain.Tests
import qualified Potato.Trader.MarketMaker.Tests
import qualified Potato.Trader.Tests

main :: IO ()
main = do
  Potato.Trader.Exchanges.Bilaxy.Tests.tests
  Potato.Trader.Exchanges.Chain.Tests.tests
  Potato.Trader.Arbitrage.Tests.tests
  Potato.Trader.Tests.tests
  --Potato.Trader.MarketMaker.Tests.tests

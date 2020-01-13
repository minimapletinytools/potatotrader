--import           Potato.CryptoTrader

import qualified Potato.CryptoTrader.Arbitrage.Tests
import qualified Potato.CryptoTrader.Exchanges.Bilaxy.Tests
import qualified Potato.CryptoTrader.Exchanges.Chain.Tests
import qualified Potato.CryptoTrader.Tests

main :: IO ()
main = do
  Potato.CryptoTrader.Exchanges.Bilaxy.Tests.tests
  Potato.CryptoTrader.Exchanges.Chain.Tests.tests
  Potato.CryptoTrader.Arbitrage.Tests.tests
  Potato.CryptoTrader.Tests.tests

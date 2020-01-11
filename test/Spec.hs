import qualified Arbitrage.Tests
import qualified Exchanges.Bilaxy.Tests
import qualified Exchanges.Chain.Tests

main :: IO ()
main = do
  Exchanges.Bilaxy.Tests.tests
  --Exchanges.Chain.Tests.tests
  Arbitrage.Tests.tests

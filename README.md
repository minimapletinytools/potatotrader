[![CircleCI](https://circleci.com/gh/pdlla/arbitragepotato.svg?style=svg)](https://circleci.com/gh/pdlla/arbitragepotato)

# Potato Trader 🥔🥔🥔

Potato Trader is a trading library written in Haskell created for the purpose of bot trading shit coins.

Exchanges and tokens are encoded at the type level to help enforce sensible trading compile time.

For example a token on an exchange is defined by
```
class (Token t, Exchange e) => ExchangeToken t e
```
a trading pair is thus
```
class (ExchangeToken t1 e, ExchangeToken t2 e) => ExchangePair t1 t2 e
```
and given 2 trading pairs, we can arbitrage
```
arbitrage :: (ExchangePair t1 t2 e1, ExchangePair t1 t2 e2) => ...
```

Potato Trader currently supports the following trading algorithms:
- arbitrage
- market making

And supports the following exchanges:
- Uniswap (on any Web3 compatible blockchain)
- [Bilaxy](https://www.bilaxy.com/)

Once an exchange is implemented, supported tokens and trading pairs must be (easily) added in code to support trading with type safety.

Some planned features that aren't implemented yet:
- accounts are read unencrypted from a file directly inside the library. This is solved with `ExchangeAccount` type family but it hasn't been fully integrated yet
  - and since my key.txt file is in .gitignore, CircleCI is failing
    - of course, this problem could also be solved with the feature in the next bullet point
- better test cases for effectful code using [test-fixture](https://lexi-lambda.github.io/blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/)

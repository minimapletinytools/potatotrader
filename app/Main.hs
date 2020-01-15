module Main where

import           Potato.CryptoTrader
import           Potato.CryptoTrader.Exchanges.Bilaxy
import           Potato.CryptoTrader.Exchanges.Chain

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Concurrent.Chan.Synchronous
import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch                  as C
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Control.Monad.Trans                  as MTL
import           Control.Monad.Writer.Lazy
import           Data.Proxy
import qualified Data.Text                            as T


type ArbMonad t1 t2= ExchangePairT t1 t2 (WriterT ArbitrageLogs IO)
arbForever :: (ArbitrageConstraints t1 t2 e1 e2 (ArbMonad t1 t2)) => Proxy (t1,t2,e1,e2) -> Chan () -> ExchangeCtx e1 -> ExchangeCtx e2 -> IO ()
arbForever pproxy exitCh ctx1 ctx2 = do
  -- run arbitrage
  let
    ctx = (ctx1, ctx2)
    arb = arbitrage pproxy
  (_,logs) <- runWriterT $ flip runReaderT ctx arb
  mapM_ (print . T.unpack) logs
  putStrLn ""

  -- sleep for a minute
  threadDelay (floor 6e7)

  -- this is a terrible exit routine you might as well delete it...
  notDone <- isEmptyChan exitCh
  if notDone then
    -- repeat
    arbForever pproxy exitCh ctx1 ctx2
  else return ()

-- I'm sure there's a better way to do this...
exitLoop :: Chan () -> IO ()
exitLoop exitCh = do
  -- just want any character, not a line D:
  _ <- getLine
  writeChan exitCh ()

main :: IO ()
main = do
  exitCh <- newChan
  forkIO $ exitLoop exitCh
  let
    --p = Proxy :: Proxy (TT,USDT,OnChain ThunderCoreMain,Bilaxy)
    p = Proxy :: Proxy (USDT,TT,ReverseExchangePair USDT TT (OnChain ThunderCoreMain),ReverseExchangePair USDT TT Bilaxy)
  arbForever p exitCh ((),()) ((),nilKey)



--main = Potato.CryptoTrader.Exchanges.Bilaxy.recordDepth 151 10
--main = Potato.CryptoTrader.Exchanges.Bilaxy.testBalance
--main = Potato.CryptoTrader.Exchanges.Bilaxy.testDepth
--main = Potato.CryptoTrader.Exchanges.Chain.testTransaction

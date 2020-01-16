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


type ArbMonad t1 t2 = ExchangePairT t1 t2 (WriterT ArbitrageLogs IO)
arbForever :: forall t1 t2 e1 e2. (ArbitrageConstraints t1 t2 e1 e2 (ArbMonad t1 t2)) => Proxy (t1,t2,e1,e2) -> Chan () -> ExchangeCtx e1 -> ExchangeCtx e2 -> IO ()
arbForever pproxy exitCh ctx1 ctx2 = do
  -- run arbitrage
  putStrLn "=====BEGIN ARBITRAGE ROUND====="
  let
    ctx = (ctx1, ctx2)
    arb = arbitrage pproxy False
  (oss,logs) <- runWriterT $ runReaderT arb ctx
  mapM_ (print . T.unpack) logs
  osl <- flip runReaderT ctx $ do
    case oss of
      Nothing -> return Nothing
      Just (eo1,eo2) -> do
        osl1 <- lifte1 $ getStatus (Proxy :: Proxy (t1,t2,e1)) eo1
        osl2 <- lifte2 $ getStatus (Proxy :: Proxy (t1,t2,e2)) eo2
        return $ Just (osl1,osl2)
  case osl of
    Nothing -> print "no orders made"
    Just (osl1,osl2) -> do
      putStrLn "=====ORDER STATUS====="
      print osl1
      print osl2
  putStrLn "=====END ARBITRAGE ROUND====="

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

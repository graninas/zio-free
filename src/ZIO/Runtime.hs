module ZIO.Runtime where

import           ZIO.Prelude
import           Control.Exception (throwIO)

import qualified ZIO.Types as T

data ZIORuntime = ZIORuntime
    { _dummy :: Int
    }

createZIORuntime :: IO ZIORuntime
createZIORuntime = pure $ ZIORuntime 0

clearZIORuntime :: ZIORuntime -> IO ()
clearZIORuntime _ = pure ()

withZIORuntime :: (ZIORuntime -> IO a) -> IO a
withZIORuntime zioRtAct =
  bracket createZIORuntime clearZIORuntime zioRtAct


relayAsyncVar :: T.Async a -> MVar a -> IO ()
relayAsyncVar inputAsyncVar outputVar =
  case inputAsyncVar of
    T.Ready val -> putMVar outputVar val
    T.Async conv eVar -> do
      eVal <- readMVar eVar
      case eVal of
        Left err -> throwIO err
        Right val -> putMVar outputVar $ conv val

awaitAsyncVar :: T.Async a -> IO a
awaitAsyncVar (T.Ready val) = pure val
awaitAsyncVar (T.Async conv eVar) = do
  eVal <- readMVar eVar
  case eVal of
    Left err -> throwIO err
    Right val -> pure $ conv val

relayMVar :: MVar a -> MVar a -> IO ()
relayMVar inputVar outputVar = do
  val <- readMVar inputVar
  putMVar outputVar val

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

relayMVar :: MVar a -> MVar a -> IO ()
relayMVar inputVar outputVar = do
  val <- readMVar inputVar
  putMVar outputVar val

relayAsyncVar :: T.Async a -> MVar (Either SomeException a) -> IO ()
relayAsyncVar inputAsyncVar outputVar =
  case inputAsyncVar of
    T.Ready val -> putMVar outputVar $ Right val
    T.Async conv eVar -> do
      eVal <- readMVar eVar
      putMVar outputVar $ case eVal of
        Left err  -> Left err
        Right val -> Right $ conv val

awaitAsyncVar :: T.Async a -> IO a
awaitAsyncVar (T.Ready val) = pure val
awaitAsyncVar (T.Async conv eVar) = do
  eVal <- readMVar eVar
  case eVal of
    Left err -> throwIO err
    Right val -> pure $ conv val

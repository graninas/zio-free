module ZIO.Runtime where

import           ZIO.Prelude

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
    T.Async conv var -> do
      val <- readMVar var
      putMVar outputVar $ conv val

awaitAsyncVar :: T.Async a -> IO a
awaitAsyncVar (T.Ready val) = pure val
awaitAsyncVar (T.Async conv var) = conv <$> readMVar var


relayMVar :: MVar a -> MVar a -> IO ()
relayMVar inputVar outputVar = do
  val <- readMVar inputVar
  putMVar outputVar val

module ZIO.Runtime where

import           ZIO.Prelude

import qualified Data.Map                        as Map

data Async a
  = Async (MVar a)
  | Ready a


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


relayAsyncVar :: Async a -> MVar a -> IO ()
relayAsyncVar inputAsyncVar outputVar =
  case inputAsyncVar of
    Ready val -> putMVar outputVar val
    Async var -> do
      val <- takeMVar var
      putMVar outputVar val

awaitAsyncVar :: Async a -> IO a
awaitAsyncVar (Ready val) = pure val
awaitAsyncVar (Async var) = takeMVar var


relayMVar :: MVar a -> MVar a -> IO ()
relayMVar inputVar outputVar = do
  val <- takeMVar inputVar
  putMVar outputVar val

module ZIO.Runtime where

import           ZIO.Prelude

import qualified Data.Map                        as Map

newtype Delayed a = Delayed (MVar a)


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

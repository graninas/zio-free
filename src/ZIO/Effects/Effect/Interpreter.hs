{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.Effect.Interpreter where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.Console.Interpreter as R
import qualified ZIO.Effects.IO.Interpreter as R
import qualified ZIO.Runtime as R
import qualified ZIO.Types as T


-----------------------------------------

-- Running async effect asynchronously.

interpretAsyncEffectF
  :: R.ZIORuntime
  -> L.EffectF T.Async (L.AsyncEffect a)
  -> IO (T.Async a)
interpretAsyncEffectF rt (L.EvalIOEff ioEff next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- R.runIOEff rt ioEff
    putMVar var r
  runAsyncEffect rt $ next $ T.Async var

interpretAsyncEffectF rt (L.EvalConsole consoleAct next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- R.runConsole rt consoleAct
    putMVar var r
  runAsyncEffect rt $ next $ T.Async var

interpretAsyncEffectF rt (L.Async asyncEff next) = do
  asyncVar <- runAsyncEffect rt asyncEff
  runAsyncEffect rt $ next $ asyncVar

interpretAsyncEffectF rt (L.Await (T.Async var) next) = do
  val <- readMVar var
  runAsyncEffect rt $ next val

interpretAsyncEffectF rt (L.Await (T.Ready val) next) =
  runAsyncEffect rt $ next val

runAsyncEffect :: R.ZIORuntime -> L.AsyncEffect a -> IO (T.Async a)
runAsyncEffect rt (Pure v) = pure $ T.Ready v
runAsyncEffect rt (Free f) = interpretAsyncEffectF rt f

-----------------------------------------

-- Running an async effect synchronously.

interpretAsyncEffectFSynchronously
  :: R.ZIORuntime
  -> L.EffectF T.Async (L.AsyncEffect a)
  -> IO (T.Async a)
interpretAsyncEffectFSynchronously rt (L.EvalIOEff ioEff next) = do
  val <- R.runIOEff rt ioEff
  runAsyncEffectSynchronously rt $ next $ T.Ready val

interpretAsyncEffectFSynchronously rt (L.EvalConsole consoleAct next) = do
  val <- R.runConsole rt consoleAct
  runAsyncEffectSynchronously rt $ next $ T.Ready val

interpretAsyncEffectFSynchronously rt (L.Async asyncEff next) = do
  asyncVar <- runAsyncEffectSynchronously rt asyncEff
  runAsyncEffectSynchronously rt $ next $ asyncVar

interpretAsyncEffectFSynchronously rt (L.Await (T.Async var) next) = do
  val <- readMVar var
  runAsyncEffectSynchronously rt $ next val

interpretAsyncEffectFSynchronously rt (L.Await (T.Ready val) next) =
  runAsyncEffectSynchronously rt $ next val

runAsyncEffectSynchronously :: R.ZIORuntime -> L.AsyncEffect a -> IO (T.Async a)
runAsyncEffectSynchronously rt (Pure v) = pure $ T.Ready v
runAsyncEffectSynchronously rt (Free f) = interpretAsyncEffectFSynchronously rt f

-------------------------------------------

-- Running a regular effect synchronously.

interpretEffectF :: R.ZIORuntime -> L.EffectF Identity a -> IO a
interpretEffectF rt (L.EvalConsole consoleAct next) =
  next . Identity <$> R.runConsole rt consoleAct

interpretEffectF rt (L.EvalIOEff ioEff next) =
  next . Identity <$> R.runIOEff rt ioEff

interpretEffectF rt (L.Async asyncEff next) =
  next <$> runAsyncEffect rt asyncEff

interpretEffectF rt (L.Await (T.Async var) next) =
  next <$> readMVar var

interpretEffectF rt (L.Await (T.Ready val) next) =
  pure $ next val

runEffect :: R.ZIORuntime -> L.Effect a -> IO a
runEffect rt = foldFree (interpretEffectF rt)

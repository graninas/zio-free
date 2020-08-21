{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.Effect.Interpreter where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.Console.Interpreter as R
import qualified ZIO.Effects.IO.Interpreter as R
import qualified ZIO.Runtime as R
import qualified ZIO.Types as T

runSyncIO = R.runIOEff
runSyncConsole = R.runConsole

interpretEffectFAsync
  :: R.ZIORuntime
  -> L.EffectF T.Async (L.AsyncEffect a)
  -> IO (IO (T.Async a))
interpretEffectFAsync rt (L.RunIOEff ioEff next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- runSyncIO rt ioEff
    putMVar var r
  pure $ runAsyncEffect rt $ next $ T.Async id var

interpretEffectFAsync rt (L.RunConsole consoleAct next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- runSyncConsole rt consoleAct
    putMVar var r
  pure $ runAsyncEffect rt $ next $ T.Async id var

interpretEffectFAsync rt (L.Async asyncEff next) = do
  asyncVar <- runAsyncEffect rt asyncEff
  pure $ runAsyncEffect rt $ next $ asyncVar

interpretEffectFAsync rt (L.Await (T.Async conv var) next) = do
  val <- readMVar var
  pure $ runAsyncEffect rt $ next $ conv val

interpretEffectFAsync rt (L.Await (T.Ready val) next) =
  pure $ runAsyncEffect rt $ next val

runAsyncEffect :: R.ZIORuntime -> L.AsyncEffect a -> IO (T.Async a)
runAsyncEffect rt (Pure v) = pure $ T.Ready v
runAsyncEffect rt (Free f) = join $ interpretEffectFAsync rt f

-----------------------------------------

interpretEffectFAsyncSynchronously
  :: R.ZIORuntime
  -> L.EffectF T.Async (L.AsyncEffect a)
  -> IO (IO (T.Async a))
interpretEffectFAsyncSynchronously rt (L.RunIOEff ioEff next) = do
  val <- runSyncIO rt ioEff
  pure $ runAsyncEffectSynchronously rt $ next $ T.Ready val

interpretEffectFAsyncSynchronously rt (L.RunConsole consoleAct next) = do
  val <- runSyncConsole rt consoleAct
  pure $ runAsyncEffectSynchronously rt $ next $ T.Ready val

interpretEffectFAsyncSynchronously rt (L.Async asyncEff next) = do
  asyncVar <- runAsyncEffectSynchronously rt asyncEff
  pure $ runAsyncEffectSynchronously rt $ next $ asyncVar

interpretEffectFAsyncSynchronously rt (L.Await (T.Async conv var) next) = do
  val <- readMVar var
  pure $ runAsyncEffectSynchronously rt $ next $ conv val

interpretEffectFAsyncSynchronously rt (L.Await (T.Ready val) next) =
  pure $ runAsyncEffectSynchronously rt $ next val

runAsyncEffectSynchronously :: R.ZIORuntime -> L.AsyncEffect a -> IO (T.Async a)
runAsyncEffectSynchronously rt (Pure v) = pure $ T.Ready v
runAsyncEffectSynchronously rt (Free f) = join $ interpretEffectFAsyncSynchronously rt f

-------------------------------------------

interpretEffectF :: R.ZIORuntime -> L.EffectF Identity a -> IO a
interpretEffectF rt (L.RunConsole consoleAct next) =
  next . Identity <$> R.runConsole rt consoleAct

interpretEffectF rt (L.RunIOEff ioEff next) =
  next . Identity <$> R.runIOEff rt ioEff

interpretEffectF rt (L.Async asyncEff next) =
  next <$> runAsyncEffect rt asyncEff

interpretEffectF rt (L.Await (T.Async conv var) next) =
  next . conv <$> readMVar var

interpretEffectF rt (L.Await (T.Ready val) next) =
  pure $ next val

runEffect :: R.ZIORuntime -> L.Effect a -> IO a
runEffect rt = foldFree (interpretEffectF rt)

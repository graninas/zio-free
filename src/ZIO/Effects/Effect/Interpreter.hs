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
  -> L.EffectF T.Async (L.EffectAsync a)
  -> IO (IO (T.Async a))
interpretEffectFAsync rt (L.RunIOEff ioEff next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- runSyncIO rt ioEff
    putMVar var r
  pure $ runEffectAsync rt $ next $ T.Async var

interpretEffectFAsync rt (L.RunConsole consoleAct next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- runSyncConsole rt consoleAct
    putMVar var r
  pure $ runEffectAsync rt $ next $ T.Async var

interpretEffectFAsync rt (L.Await (T.Async var) next) = do
  val <- takeMVar var
  pure $ runEffectAsync rt $ next val

interpretEffectFAsync rt (L.Await (T.Ready val) next) =
  pure $ runEffectAsync rt $ next val

runEffectAsync :: R.ZIORuntime -> L.EffectAsync a -> IO (T.Async a)
runEffectAsync rt (Pure v) = pure $ T.Ready v
runEffectAsync rt (Free f) = do
  act <- interpretEffectFAsync rt f
  var <- newEmptyMVar
  void $ forkIO $ do
    asyncVar <- act
    case asyncVar of
      T.Async var' -> do
        val' <- takeMVar var'
        putMVar var val'
      T.Ready val' -> putMVar var val'
  pure $ T.Async var

-----------------------------------------

interpretEffectFAsyncSynchronously
  :: R.ZIORuntime
  -> L.EffectF T.Async (L.EffectAsync a)
  -> IO (IO (T.Async a))
interpretEffectFAsyncSynchronously rt (L.RunIOEff ioEff next) = do
  val <- runSyncIO rt ioEff
  pure $ runAsyncEffectSynchronously rt $ next $ T.Ready val

interpretEffectFAsyncSynchronously rt (L.RunConsole consoleAct next) = do
  val <- runSyncConsole rt consoleAct
  pure $ runAsyncEffectSynchronously rt $ next $ T.Ready val

interpretEffectFAsyncSynchronously rt (L.Await (T.Async var) next) = do
  val <- takeMVar var
  pure $ runAsyncEffectSynchronously rt $ next val

interpretEffectFAsyncSynchronously rt (L.Await (T.Ready val) next) =
  pure $ runAsyncEffectSynchronously rt $ next val

runAsyncEffectSynchronously :: R.ZIORuntime -> L.EffectAsync a -> IO (T.Async a)
runAsyncEffectSynchronously rt (Pure v) = pure $ T.Ready v
runAsyncEffectSynchronously rt (Free f) = do
  act <- interpretEffectFAsyncSynchronously rt f
  act

-------------------------------------------

interpretEffectF :: R.ZIORuntime -> L.EffectF Identity a -> IO a
interpretEffectF rt (L.RunConsole consoleAct next) =
  next . Identity <$> R.runConsole rt consoleAct

interpretEffectF rt (L.RunIOEff ioEff next) =
  next . Identity <$> R.runIOEff rt ioEff

interpretEffectF rt (L.Await (T.Async var) next) =
  next <$> takeMVar var

interpretEffectF rt (L.Await (T.Ready val) next) =
  pure $ next val

runEffect :: R.ZIORuntime -> L.Effect a -> IO a
runEffect rt = foldFree (interpretEffectF rt)

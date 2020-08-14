{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.Effect.Interpreter where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.Console.Interpreter as R
import qualified ZIO.Effects.IO.Interpreter as R
import qualified ZIO.Runtime as R


interpretEffectFAsync rt (L.RunIOEff ioEff next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- R.runIOEff rt ioEff
    putMVar var r
  pure $ do
    val <- takeMVar var
    runEffectAsync rt $ next val

interpretEffectFAsync rt (L.RunConsole consoleAct next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- R.runConsole rt consoleAct
    putMVar var r
  pure $ do
    val <- takeMVar var
    runEffectAsync rt $ next val


runEffectAsync rt (Pure val) = pure $ R.Ready val
runEffectAsync rt (Free f) = do
  act <- interpretEffectFAsync rt f
  var <- newEmptyMVar
  void $ forkIO $ do
    asyncVar <- act
    R.relayAsyncVar asyncVar var
  pure $ R.Async var



interpretEffectF :: R.ZIORuntime -> L.EffectF a -> IO a
interpretEffectF rt (L.RunConsole consoleAct next) =
  next <$> R.runConsole rt consoleAct

interpretEffectF rt (L.RunIOEff ioEff next) =
  next <$> R.runIOEff rt ioEff

runEffect :: R.ZIORuntime -> L.Effect a -> IO a
runEffect rt = foldFree (interpretEffectF rt)

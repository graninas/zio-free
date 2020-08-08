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

-- interpretEffectFAsync :: R.ZIORuntime -> L.EffectF a -> IO a
-- interpretEffectFAsync rt (L.RunIOEff ioEff next) = do
--   var <- newEmptyMVar
--   void $ forkIO $ do
--     r <- runSyncIO rt ioEff
--     putMVar var r
--   pure $ do
--     val <- takeMVar var
--     runEffectAsync rt $ next val

-- interpretEffectFAsync rt (L.RunConsole consoleAct next) = do
--   var <- newEmptyMVar
--   void $ forkIO $ do
--     r <- runSyncConsole rt consoleAct
--     putMVar var r
--   pure $ do
--     val <- takeMVar var
--     runEffectAsync rt $ next val

-- runEffectAsync :: R.ZIORuntime -> L.Effect a -> IO (Delayed a)
-- runEffectAsync rt (Pure v) = T.Delayed <$> newMVar v
-- runEffectAsync rt (Free f) = do
--   act <- interpretEffectFAsync rt f
--   var <- newEmptyMVar
--   void $ forkIO $ do
--     T.Delayed var' <- act
--     val <- takeMVar var'
--     putMVar var val
--   pure $ T.Delayed var



interpretEffectF :: R.ZIORuntime -> L.EffectF Identity a -> IO a
interpretEffectF rt (L.RunConsole consoleAct next) =
  next . Identity <$> R.runConsole rt consoleAct

interpretEffectF rt (L.RunIOEff ioEff next) =
  next . Identity <$> R.runIOEff rt ioEff

runEffect :: R.ZIORuntime -> L.Effect a -> IO a
runEffect rt = foldFree (interpretEffectF rt)

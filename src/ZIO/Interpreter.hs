{-# LANGUAGE BangPatterns #-}

module ZIO.Interpreter where

import           ZIO.Prelude

import qualified Data.Map as Map

import qualified ZIO.Language as L
import qualified ZIO.Runtime  as R
import qualified ZIO.Effects.Effect.Interpreter as R

-- interpretZIOFAsync :: R.ZIORuntime -> L.ZIOF a -> IO a
interpretZIOFAsync rt (L.RunEffect eff next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    R.Delayed var' <- R.runEffectAsync rt eff
    val <- takeMVar var'
    putMVar var val
  pure $ do
    val <- takeMVar var
    runZIOAsync rt $ next val

interpretZIOFAsync rt (L.RunSync eff next) = error "interpretZIOFAsync RunSync not implemented."
interpretZIOFAsync rt (L.RunAsync eff next) = error "interpretZIOFAsync RunAsync not implemented."

-- runZIOAsync :: R.ZIORuntime -> L.ZIO a -> IO (R.Delayed a)
runZIOAsync rt (Pure v) = R.Delayed <$> newMVar v
runZIOAsync rt (Free f) = do
  act <- interpretZIOFAsync rt f
  var <- newEmptyMVar
  void $ forkIO $ do
    R.Delayed var' <- act
    val <- takeMVar var'
    putMVar var val
  pure $ R.Delayed var




interpretZIOFSync :: R.ZIORuntime -> L.ZIOF a -> IO a
interpretZIOFSync rt (L.RunEffect eff next) = do
  r <- R.runEffect rt eff
  pure $ next r

interpretZIOFSync rt (L.RunSync eff next) = do
  r <- runZIOSync rt eff
  pure $ next r

interpretZIOFSync rt (L.RunAsync eff next) = do
  R.Delayed var <- runZIOAsync rt eff
  r <- takeMVar var
  pure $ next r

runZIOSync :: R.ZIORuntime -> L.ZIO a -> IO a
runZIOSync rt = foldFree (interpretZIOFSync rt)

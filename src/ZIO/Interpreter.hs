{-# LANGUAGE BangPatterns #-}

module ZIO.Interpreter where

import           ZIO.Prelude

import qualified Data.Map as Map

import qualified ZIO.Language as L
import qualified ZIO.Runtime  as R
import qualified ZIO.Effects.Effect.Interpreter as R

interpretZIOFAsync :: R.ZIORuntime -> L.ZIOF (L.ZIO a) -> IO (IO (MVar a))
interpretZIOFAsync rt (L.RunEffect eff next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    var' <- R.runEffectAsync rt eff
    R.relayMVar var' var
  pure $ do
    val <- takeMVar var
    runZIOAsync rt $ next val

interpretZIOFAsync rt (L.RunSync eff next) = error "interpretZIOFAsync RunSync not implemented."
interpretZIOFAsync rt (L.RunAsync eff next) = error "interpretZIOFAsync RunAsync not implemented."

runZIOAsync :: R.ZIORuntime -> L.ZIO a -> IO (MVar a)
runZIOAsync rt (Pure val) = newMVar val
runZIOAsync rt (Free f) = do
  act <- interpretZIOFAsync rt f
  var <- newEmptyMVar
  void $ forkIO $ do
    var' <- act
    R.relayMVar var' var
  pure var




interpretZIOF :: R.ZIORuntime -> L.ZIOF a -> IO a
interpretZIOF rt (L.RunEffect eff next) = do
  r <- R.runEffect rt eff
  pure $ next r

interpretZIOF rt (L.RunSync eff next) = do
  r <- runZIOSync rt eff
  pure $ next r

interpretZIOF rt (L.RunAsync eff next) = do
  var <- runZIOAsync rt eff
  val <- takeMVar var
  pure $ next val

runZIOSync :: R.ZIORuntime -> L.ZIO a -> IO a
runZIOSync rt = foldFree (interpretZIOF rt)


runZIO :: R.ZIORuntime -> L.ZIO a -> IO a
runZIO rt (Pure val) = pure val
runZIO rt (Free act) = do
  next <- interpretZIOF rt act
  runZIO rt next

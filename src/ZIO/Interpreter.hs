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
    asyncVar <- R.runEffectAsync rt eff
    R.relayAsyncVar asyncVar var
  pure $ do
    val <- takeMVar var
    runZIOAsync rt $ next val

interpretZIOFAsync rt (L.RunSync eff next) = error "interpretZIOFAsync RunSync not implemented."
interpretZIOFAsync rt (L.RunAsync eff next) = error "interpretZIOFAsync RunAsync not implemented."

-- runZIOAsync :: R.ZIORuntime -> L.ZIO a -> IO (R.Delayed a)
runZIOAsync rt (Pure val) = pure $ R.Ready val
runZIOAsync rt (Free f) = do
  act <- interpretZIOFAsync rt f
  var <- newEmptyMVar
  void $ forkIO $ do
    asyncVar <- act
    R.relayAsyncVar asyncVar var
  pure $ R.Async var




interpretZIOFSync :: R.ZIORuntime -> L.ZIOF a -> IO a
interpretZIOFSync rt (L.RunEffect eff next) = do
  r <- R.runEffect rt eff
  pure $ next r

interpretZIOFSync rt (L.RunSync eff next) = do
  r <- runZIOSync rt eff
  pure $ next r

interpretZIOFSync rt (L.RunAsync eff next) = do
  asyncVar <- runZIOAsync rt eff
  val <- R.awaitAsyncVar asyncVar
  pure $ next val

runZIOSync :: R.ZIORuntime -> L.ZIO a -> IO a
runZIOSync rt = foldFree (interpretZIOFSync rt)

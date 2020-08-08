{-# LANGUAGE BangPatterns #-}

module ZIO.Interpreter where

import           ZIO.Prelude

import qualified Data.Map as Map

import qualified ZIO.Language as L
import qualified ZIO.Runtime  as R
import qualified ZIO.Effects.Effect.Interpreter as R

-- interpretZIOFAsync :: R.ZIORuntime -> L.ZIOF a -> IO a
-- interpretZIOFAsync rt (L.RunEffect eff next) = do
--   var <- newEmptyMVar
--   void $ forkIO $ do
--     T.Delayed var' <- R.runEffectAsync rt eff
--     val <- takeMVar var'
--     putMVar var val
--   pure $ do
--     val <- takeMVar var
--     runZIOAsync rt $ next val
--
-- interpretZIOFAsync rt (L.RunSync eff next) = error "interpretZIOFAsync RunSync not implemented."
-- interpretZIOFAsync rt (L.RunAsync eff next) = error "interpretZIOFAsync RunAsync not implemented."

-- runZIOAsync :: R.ZIORuntime -> L.ZIO a -> IO (T.Delayed a)
-- runZIOAsync rt (Pure v) = T.Delayed <$> newMVar v
-- runZIOAsync rt (Free f) = do
--   act <- interpretZIOFAsync rt f
--   var <- newEmptyMVar
--   void $ forkIO $ do
--     T.Delayed var' <- act
--     val <- takeMVar var'
--     putMVar var val
--   pure $ T.Delayed var




interpretZIOFSync :: R.ZIORuntime -> L.ZIOF Identity a -> IO a
interpretZIOFSync rt (L.RunEffect eff next) =
  next . Identity <$> R.runEffect rt eff

interpretZIOFSync rt (L.RunSync eff next) = do
  next . Identity <$> runZIOSync rt eff

-- interpretZIOFSync rt (L.RunAsync eff next) = do
--   T.Delayed var <- runZIOAsync rt eff
--   r <- takeMVar var
--   pure $ next r

runZIOSync :: R.ZIORuntime -> L.ZIO a -> IO a
runZIOSync rt = foldFree (interpretZIOFSync rt)

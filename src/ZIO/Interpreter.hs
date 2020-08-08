{-# LANGUAGE BangPatterns #-}

module ZIO.Interpreter where

import           ZIO.Prelude

import qualified Data.Map as Map

import qualified ZIO.Language as L
import qualified ZIO.Runtime  as R
import qualified ZIO.Types  as T
import qualified ZIO.Effects.Effect.Interpreter as R


interpretZIOF :: R.ZIORuntime -> L.ZIOF Identity a -> IO a
interpretZIOF rt (L.RunSync eff next) = do
  next . Identity <$> R.runEffect rt eff

interpretZIOF rt (L.RunAsync eff next) = do
  T.Async var <- R.runEffectAsync rt eff
  r <- takeMVar var
  pure $ next $ Identity r

runZIO :: R.ZIORuntime -> L.ZIO a -> IO a
runZIO rt = foldFree (interpretZIOF rt)

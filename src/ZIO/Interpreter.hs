{-# LANGUAGE BangPatterns #-}

module ZIO.Interpreter where

import           ZIO.Prelude

import qualified Data.Map as Map

import qualified ZIO.Language as L
import qualified ZIO.Runtime  as R
import qualified ZIO.Types  as T
import qualified ZIO.Effects.Effect.Interpreter as R


interpretZIOF :: R.ZIORuntime -> L.ZIOF a -> IO a
interpretZIOF rt (L.RunSync eff next) = do
  next <$> R.runEffect rt eff

interpretZIOF rt (L.RunAsync eff next) = do
  asyncVar <- R.runEffectAsync rt eff
  case asyncVar of
    T.Async var -> next <$> takeMVar var
    T.Ready val -> pure $ next val

interpretZIOF rt (L.Await' (T.Async var) next) =
  next <$> takeMVar var

interpretZIOF rt (L.Await' (T.Ready val) next) =
  pure $ next val

runZIO :: R.ZIORuntime -> L.ZIO a -> IO a
runZIO rt = foldFree (interpretZIOF rt)

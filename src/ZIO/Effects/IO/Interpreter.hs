{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.IO.Interpreter where

import           ZIO.Prelude

import qualified Data.Map as Map

import qualified ZIO.Effects.IO.Language as L
import qualified ZIO.Runtime as R


interpretIOEffF :: R.ZIORuntime -> L.IOF a -> IO a
interpretIOEffF _ (L.RunIO f next) = do
  !r <- f
  pure $ next r

runIOEff :: R.ZIORuntime -> L.IOEff a -> IO a
runIOEff rt = foldFree (interpretIOEffF rt)

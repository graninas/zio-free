{-# LANGUAGE BangPatterns #-}

module ZIO.Interpreter where

import           ZIO.Prelude

import qualified Data.Map as Map

import qualified ZIO.Language as L
import qualified ZIO.Runtime  as R


interpretZIOF :: R.ZIORuntime -> L.ZIOF a -> IO a
interpretZIOF _ (L.EvalIO f next) = do
  !r <- f
  pure $ next r

runZIO :: R.ZIORuntime -> L.ZIO a -> IO a
runZIO rt = foldFree (interpretZIOF rt)

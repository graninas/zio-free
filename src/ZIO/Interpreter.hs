{-# LANGUAGE BangPatterns #-}

module ZIO.Interpreter where

import           ZIO.Prelude
import Control.Exception (throwIO)

import qualified Data.Map as Map

import qualified ZIO.Language as L
import qualified ZIO.Runtime  as R
import qualified ZIO.Types    as T
import qualified ZIO.Effects.Effect.Interpreter as R

interpretZIOF :: R.ZIORuntime -> L.ZIOF a -> IO a

interpretZIOF rt (L.EvalAsyncEffect eff next) = do
  asyncVar <- R.runAsyncEffect rt eff
  next <$> R.awaitAsyncVar asyncVar

-- interpretZIOF rt (L.EvalSynchronously eff next) = do
--   asyncVar <- R.runAsyncEffectSynchronously rt eff
--   next <$> R.awaitAsyncVar asyncVar

interpretZIOF rt (L.EvalEffect eff next) =
  next <$> R.runEffect rt eff

interpretZIOF rt (L.EvalSafely act next) =
  next <$> (try $ runZIO rt act)

runZIO :: R.ZIORuntime -> L.ZIO a -> IO a
runZIO rt = foldFree (interpretZIOF rt)

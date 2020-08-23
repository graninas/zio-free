{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.Effect.Interpreter where

import           ZIO.Prelude

import           Control.Exception (throwIO)
import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.Console.Interpreter as R
import qualified ZIO.Effects.IO.Interpreter as R
import qualified ZIO.Runtime as R
import qualified ZIO.Types as T


-------------------------------------------

-- Running a regular effect synchronously.

interpretEffectF :: R.ZIORuntime -> L.EffectF a -> IO a
interpretEffectF rt (L.EvalConsole consoleAct next) =
  next . Identity <$> R.runConsole rt consoleAct

interpretEffectF rt (L.EvalIOEff ioEff next) =
  next . Identity <$> R.runIOEff rt ioEff

interpretEffectF rt (L.EvalEffectSafely eff next) =
  next <$> (try $ runEffect rt eff)

runEffect :: R.ZIORuntime -> L.Effect a -> IO a
runEffect rt = foldFree (interpretEffectF rt)


-----------------------------------------

-- Running async effect asynchronously.

interpretAsyncEffectF
  :: R.ZIORuntime
  -> L.AsyncEffectF (L.AsyncEffect a)
  -> IO (T.Async a)

interpretAsyncEffectF rt (L.EvalEffect eff next) = do
  val <- runEffect rt eff
  runAsyncEffect rt $ next val

interpretAsyncEffectF rt (L.Async asyncEff next) = do
  eVar <- newEmptyMVar
  void $ forkIO $ do
    asyncVar <- runAsyncEffect rt asyncEff         -- FIXME: try
    R.relayAsyncVar asyncVar eVar
  runAsyncEffect rt $ next $ T.Async id eVar

--
-- FIXME: safe
-- interpretAsyncEffectF rt (L.Await (T.Async conv eVar) next) = do
--   eVal <- readMVar eVar
--   case eVal of
--     Left err  -> throwIO err
--     Right val -> runAsyncEffect rt $ next $ conv val
interpretAsyncEffectF rt (L.Await (T.Async conv eVar) next) = do
  eVal <- readMVar eVar
  runAsyncEffect rt $ next $ conv eVal

interpretAsyncEffectF rt (L.Await (T.Ready val) next) =
  runAsyncEffect rt $ next val


runAsyncEffect :: R.ZIORuntime -> L.AsyncEffect a -> IO (T.Async a)
runAsyncEffect rt (Pure v) = pure $ T.Ready v
runAsyncEffect rt (Free f) = interpretAsyncEffectF rt f

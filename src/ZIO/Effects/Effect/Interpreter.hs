{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.Effect.Interpreter where

import           ZIO.Prelude

import           Control.Exception (throwIO)
import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.Console.Interpreter as R
import qualified ZIO.Effects.IO.Interpreter as R
import qualified ZIO.Runtime as R
import qualified ZIO.Types as T


-----------------------------------------

-- Running async effect asynchronously.

interpretAsyncEffectF
  :: R.ZIORuntime
  -> L.EffectF T.Async (L.AsyncEffect a)
  -> IO (T.Async a)
interpretAsyncEffectF rt (L.EvalIOEff ioEff next) = do
  eVar <- newEmptyMVar
  void $ forkIO $ do
    er <- try $ R.runIOEff rt ioEff
    putMVar eVar er
  runAsyncEffect rt $ next $ T.Async id eVar

interpretAsyncEffectF rt (L.EvalConsole consoleAct next) = do
  eVar <- newEmptyMVar
  void $ forkIO $ do
    er <- try $ R.runConsole rt consoleAct
    putMVar eVar er
  runAsyncEffect rt $ next $ T.Async id eVar

interpretAsyncEffectF rt (L.EvalAsyncEffectInAsync asyncEff next) = do
  asyncVar <- runAsyncEffect rt asyncEff
  runAsyncEffect rt $ next asyncVar

interpretAsyncEffectF rt (L.Await (T.Async conv eVar) next) = do
  eVal <- readMVar eVar
  case eVal of
    Left err  -> throwIO err
    Right val -> runAsyncEffect rt $ next $ conv val

interpretAsyncEffectF rt (L.Await (T.Ready val) next) =
  runAsyncEffect rt $ next val

interpretAsyncEffectF rt (L.ThrowExceptionEff exc _) = throwIO exc

-- interpretAsyncEffectF rt (L.EvalSafelyAsyncEffect act next) = do
--   eResult <- try $ runAsyncEffect rt act
--   runAsyncEffect rt $ next $ case eResult of
--     Left err -> Left err
--     Right r  -> Right r
--
-- interpretAsyncEffectF rt (L.EvalSafelyEffect act next) = do
--   eResult <- try $ runEffect rt act
--   pure $ next $ case eResult of
--     Left err -> Left err
--     Right r  -> Right r


runAsyncEffect :: R.ZIORuntime -> L.AsyncEffect a -> IO (T.Async a)
runAsyncEffect rt (Pure v) = pure $ T.Ready v
runAsyncEffect rt (Free f) = interpretAsyncEffectF rt f

-----------------------------------------

-- Running an async effect synchronously.

interpretAsyncEffectFSynchronously
  :: R.ZIORuntime
  -> L.EffectF T.Async (L.AsyncEffect a)
  -> IO (T.Async a)
interpretAsyncEffectFSynchronously rt (L.EvalIOEff ioEff next) = do
  val <- R.runIOEff rt ioEff
  runAsyncEffectSynchronously rt $ next $ T.Ready val

interpretAsyncEffectFSynchronously rt (L.EvalConsole consoleAct next) = do
  val <- R.runConsole rt consoleAct
  runAsyncEffectSynchronously rt $ next $ T.Ready val

interpretAsyncEffectFSynchronously rt (L.EvalAsyncEffectInAsync asyncEff next) = do
  asyncVar <- runAsyncEffectSynchronously rt asyncEff
  runAsyncEffectSynchronously rt $ next $ asyncVar

interpretAsyncEffectFSynchronously rt (L.Await (T.Async conv eVar) next) = do
  eVal <- readMVar eVar
  case eVal of
    Left err  -> throwIO err
    Right val -> runAsyncEffectSynchronously rt $ next $ conv val

interpretAsyncEffectFSynchronously rt (L.Await (T.Ready val) next) =
  runAsyncEffectSynchronously rt $ next val

interpretAsyncEffectFSynchronously rt (L.ThrowExceptionEff exc _) = throwIO exc

-- interpretAsyncEffectFSynchronously rt (L.EvalSafelyAsyncEffect act next) = do
--   eResult <- try $ runAsyncEffectSynchronously rt act
--   pure $ next $ case eResult of
--     Left err -> Left err
--     Right r  -> Right r
--
-- interpretAsyncEffectFSynchronously rt (L.EvalSafelyEffect act next) = do
--   eResult <- try $ runEffect rt act
--   pure $ next $ case eResult of
--     Left err -> Left err
--     Right r  -> Right r

runAsyncEffectSynchronously :: R.ZIORuntime -> L.AsyncEffect a -> IO (T.Async a)
runAsyncEffectSynchronously rt (Pure v) = pure $ T.Ready v
runAsyncEffectSynchronously rt (Free f) = interpretAsyncEffectFSynchronously rt f

-------------------------------------------

-- Running a regular effect synchronously.

interpretEffectF :: R.ZIORuntime -> L.EffectF Identity a -> IO a
interpretEffectF rt (L.EvalConsole consoleAct next) =
  next . Identity <$> R.runConsole rt consoleAct

interpretEffectF rt (L.EvalIOEff ioEff next) =
  next . Identity <$> R.runIOEff rt ioEff

interpretEffectF rt (L.EvalAsyncEffectInAsync asyncEff next) =
  next <$> runAsyncEffect rt asyncEff

interpretEffectF rt (L.Await (T.Async conv eVar) next) = do
  eVal <- readMVar eVar
  case eVal of
    Left err  -> throwIO err
    Right val -> pure $ next $ conv val

interpretEffectF rt (L.Await (T.Ready val) next) =
  pure $ next val

interpretEffectF rt (L.ThrowExceptionEff exc _) = throwIO exc

-- interpretEffectF rt (L.EvalSafelyAsyncEffect act next) = do
--   eResult <- try $ runAsyncEffectSynchronously rt act
--   pure $ next $ case eResult of
--     Left err -> Left err
--     Right r  -> Right r
--
-- interpretEffectF rt (L.EvalSafelyEffect act next) = do
--   eResult <- try $ runEffect rt act
--   pure $ next $ case eResult of
--     Left err -> Left err
--     Right r  -> Right r

runEffect :: R.ZIORuntime -> L.Effect a -> IO a
runEffect rt = foldFree (interpretEffectF rt)

{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.Effect.Interpreter where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.Console.Interpreter as R
import qualified ZIO.Effects.IO.Interpreter as R
import qualified ZIO.Runtime as R

--
-- app :: Effect Int
-- app = do
--   aLine :: Delayed String <- async $ runIO P.getLine
--
--   eRes <- async $ runIO $ do
--     void $ try @SomeException $ Proc.readCreateProcess (Proc.shell "wget https://gist.github.com/graninas/01565065c18c01e88a5ebcbfbb96e397") ""
--     try @SomeException $ P.readFile "01565065c18c01e88a5ebcbfbb96e397"
--
--   withAsync (aLine, eRes) $ \(line, res) ->
--     runIO $ do
--       P.putStrLn line
--       (content, size) <- case eRes of
--         Left err -> show err
--         Right content -> content
--       P.putStrLn content
--       P.putStrLn $ show size
--       pure size



runSyncIO = R.runIOEff
runSyncConsole = R.runConsole


-- interpretEffectFAsync :: R.ZIORuntime -> L.EffectF a -> IO a
interpretEffectFAsync rt (L.RunIOEff ioEff next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- runSyncIO rt ioEff
    putMVar var r
  pure $ do
    val <- takeMVar var
    runEffectAsync rt $ next val

interpretEffectFAsync rt (L.RunConsole consoleAct next) = do
  var <- newEmptyMVar
  void $ forkIO $ do
    r <- runSyncConsole rt consoleAct
    putMVar var r
  pure $ do
    val <- takeMVar var
    runEffectAsync rt $ next val

-- interpretEffectFAsync rt (L.RunConsole consoleAct next) = do
--   var <- newEmptyMVar
--   void $ forkIO $ do
--     r <- runSyncConsole rt consoleAct
--     putMVar var r
--   pure (next, R.Delayed var)

-- runEffectAsync :: R.ZIORuntime -> L.Effect a -> IO (Delayed a)
runEffectAsync rt (Pure v) = R.Delayed <$> newMVar v
runEffectAsync rt (Free f) = do
  act <- interpretEffectFAsync rt f
  var <- newEmptyMVar
  void $ forkIO $ do
    R.Delayed var' <- act
    val <- takeMVar var'
    putMVar var val
  pure $ R.Delayed var


  -- (next, R.Delayed var') <- interpretEffectFAsync rt f
  -- var <- newEmptyMVar
  -- void $ forkIO $ do
  --   val <- takeMVar var'
  --   r <- runEffectAsync rt $ next val
  --   putMVar var r
  -- pure $ R.Delayed var


interpretEffectF :: R.ZIORuntime -> L.EffectF a -> IO a
interpretEffectF rt (L.RunConsole consoleAct next) =
  next <$> R.runConsole rt consoleAct

interpretEffectF rt (L.RunIOEff ioEff next) =
  next <$> R.runIOEff rt ioEff

runEffect :: R.ZIORuntime -> L.Effect a -> IO a
runEffect rt = foldFree (interpretEffectF rt)

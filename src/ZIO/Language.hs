{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Language where

import qualified Prelude as P
import           ZIO.Prelude hiding (putStrLn, getLine)

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Effects.IO.Language as L

data ZIOF next where
  RunAsyncEffect :: L.AsyncEffect a -> (a -> next) -> ZIOF next
  RunSynchronously :: L.AsyncEffect a -> (a -> next) -> ZIOF next
  RunEffect :: L.Effect a -> (a -> next) -> ZIOF next

instance Functor ZIOF  where
  fmap f (RunAsyncEffect asyncEff next) = RunAsyncEffect asyncEff (f . next)
  fmap f (RunSynchronously asyncEff next) = RunSynchronously asyncEff (f . next)
  fmap f (RunEffect eff next) = RunEffect eff (f . next)

type ZIO = Free ZIOF

runEffect :: L.Effect a -> ZIO a
runEffect eff = liftF $ RunEffect eff id

runAsyncEffect :: L.AsyncEffect a -> ZIO a
runAsyncEffect eff = liftF $ RunAsyncEffect eff id

runSynchronously :: L.AsyncEffect a -> ZIO a
runSynchronously eff = liftF $ RunSynchronously eff id

-- putStrLn :: String -> ZIO ()
-- putStrLn line = runEffect $ L.runConsole $ L.putStrLn line
--
-- getStrLn :: ZIO String
-- getStrLn = runEffect $ L.runConsole $ L.getStrLn

instance L.Effect' ZIO Identity where
  runConsole consoleAct = runEffect $ L.runConsole consoleAct
  runIO ioEff = runEffect $ L.runIO ioEff

instance L.Awaitable ZIO where
  await var = runEffect $ L.await var

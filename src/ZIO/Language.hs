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
  EvalAsyncEffect :: L.AsyncEffect a -> (a -> next) -> ZIOF next
  EvalSynchronously :: L.AsyncEffect a -> (a -> next) -> ZIOF next
  EvalEffect :: L.Effect a -> (a -> next) -> ZIOF next
  EvalSafely :: forall a e next. Exception e => ZIO a -> (Either e a -> next) -> ZIOF next

instance Functor ZIOF  where
  fmap f (EvalAsyncEffect asyncEff next) = EvalAsyncEffect asyncEff (f . next)
  fmap f (EvalSynchronously asyncEff next) = EvalSynchronously asyncEff (f . next)
  fmap f (EvalEffect eff next) = EvalEffect eff (f . next)
  fmap f (EvalSafely act next) = EvalSafely act (f . next)

type ZIO = Free ZIOF

evalEffect :: L.Effect a -> ZIO a
evalEffect eff = liftF $ EvalEffect eff id

evalAsyncEffect :: L.AsyncEffect a -> ZIO a
evalAsyncEffect eff = liftF $ EvalAsyncEffect eff id

evalSynchronously :: L.AsyncEffect a -> ZIO a
evalSynchronously eff = liftF $ EvalSynchronously eff id

instance L.Effect' ZIO Identity where
  evalConsole consoleAct = evalEffect $ L.evalConsole consoleAct
  evalIO ioEff = evalEffect $ L.evalIO ioEff

instance L.Awaitable ZIO where
  await var = evalEffect $ L.await var

instance L.Throw ZIO where
  throwException ex = evalEffect $ L.throwException ex

instance L.Safe ZIO where
  evalSafely act = liftF $ EvalSafely act id

zioTry :: Exception e => ZIO a -> ZIO (Either e a)
zioTry = L.evalSafely

zioThrow :: forall a e. Exception e => e -> ZIO a
zioThrow = L.throwException

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Language where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.IO.Language as L
import qualified ZIO.Types as T

data ZIOF next where
  RunAsyncEffect :: L.AsyncEffect a -> (a -> next) -> ZIOF next
  RunSynchronously :: L.AsyncEffect a -> (a -> next) -> ZIOF next
  RunEffect :: L.Effect a -> (a -> next) -> ZIOF next

  ThrowException :: forall a e next. Exception e => e -> (a -> next) -> ZIOF next
  RunSafely      :: forall a e next. Exception e => ZIO a -> (Either e a -> next) -> ZIOF next

instance Functor ZIOF  where
  fmap f (RunAsyncEffect asyncEff next) = RunAsyncEffect asyncEff (f . next)
  fmap f (RunSynchronously asyncEff next) = RunSynchronously asyncEff (f . next)
  fmap f (RunEffect eff next) = RunEffect eff (f . next)
  fmap f (ThrowException exc next) = ThrowException exc (f . next)
  fmap f (RunSafely act next) = RunSafely act (f . next)

type ZIO = Free ZIOF

runAsyncEffect :: L.AsyncEffect a -> ZIO a
runAsyncEffect eff = liftF $ RunAsyncEffect eff id

runSynchronously :: L.AsyncEffect a -> ZIO a
runSynchronously eff = liftF $ RunSynchronously eff id

runEffect :: L.Effect a -> ZIO a
runEffect eff = liftF $ RunEffect eff id

-- | Throws an exception.
throwException :: forall a e. Exception e => e -> ZIO a
throwException ex = liftF $ ThrowException ex id

-- | Catches only a specified type of exceptions or exceptions which are wider.
-- For example, when SomeException is specified, any exceptions will be catched.
-- Otherwise depends on the hierarchy of the exceptions.
runSafely :: Exception e => ZIO a -> ZIO (Either e a)
runSafely act = liftF $ RunSafely act id

-- | Catches any type of exceptions and returns as SomeException.
runSafely' :: ZIO a -> ZIO (Either SomeException a)
runSafely' = runSafely

zioTry :: Exception e => ZIO a -> ZIO (Either e a)
zioTry = runSafely

zioThrow :: forall a e. Exception e => e -> ZIO a
zioThrow = throwException


instance L.Awaitable ZIO where
  await var = runEffect $ L.await var

instance L.Asynchronous ZIO where
  async var = error "not implemented yet."

instance L.HasIO ZIO where
  runIO ioAct = runEffect $ L.runIO ioAct

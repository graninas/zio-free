{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Effects.Effect.Language where

import           ZIO.Prelude
import qualified Prelude as P
import qualified System.Process as Proc

import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Effects.IO.Language as L
import qualified ZIO.Types as T

data EffectF m next where
  EvalConsole :: L.Console a -> (m a -> next) -> EffectF m next
  EvalIOEff :: L.IOEff a -> (m a -> next) -> EffectF m next
  Async' :: AsyncEffect a -> (T.Async a -> next) -> EffectF m next
  Await :: T.Async a -> (a -> next) -> EffectF m next

  ThrowExceptionEff :: forall m a e next. Exception e
    => e
    -> (a -> next)
    -> EffectF m next

  EvalSafelyAsyncEffect :: forall m a next
    . AsyncEffect a
   -> (T.Async a -> next)
   -> EffectF m next

  EvalSafelyEffect :: forall m a e next. Exception e
   => Effect a
   -> (Either e a -> next)
   -> EffectF m next


type Effect = Free (EffectF Identity)
type AsyncEffect = Free (EffectF T.Async)

instance Functor (EffectF m) where
  fmap f (EvalConsole consoleAct next) = EvalConsole consoleAct (f . next)
  fmap f (EvalIOEff ioEff next) = EvalIOEff ioEff (f . next)
  fmap f (Async' asyncEff next) = Async' asyncEff (f . next)
  fmap f (Await var next) = Await var (f . next)
  fmap f (ThrowExceptionEff exc next) = ThrowExceptionEff exc (f . next)
  fmap f (EvalSafelyAsyncEffect act next) = EvalSafelyAsyncEffect act (f . next)
  fmap f (EvalSafelyEffect act next) = EvalSafelyEffect act (f . next)

----------------

class Awaitable m where
  await :: T.Async a -> m a

class Asynchronous m where
  async :: AsyncEffect a -> m (T.Async a)

class Awaitable m => Effect' m mode | m -> mode where
  evalConsole :: L.Console a -> m (mode a)
  evalIO :: IO a -> m (mode a)

class Throw m where
  throwException :: forall a e. Exception e => e -> m a

class Safe m where
  -- | Catches only a specified type of exceptions or exceptions which are wider.
  -- For example, when SomeException is specified, any exceptions will be catched.
  -- Otherwise depends on the hierarchy of the exceptions.
  evalSafely :: Exception e => m a -> m (Either e a)

-- | Catches any type of exceptions and returns as SomeException.
evalSafely' :: Safe m => m a -> m (Either SomeException a)
evalSafely' = evalSafely

-------------------------

instance Throw Effect where
  throwException ex = liftF $ ThrowExceptionEff ex id

instance Throw AsyncEffect where
  throwException ex = liftF $ ThrowExceptionEff ex id

instance Safe Effect where
  evalSafely act = liftF $ EvalSafelyEffect act id

instance Safe AsyncEffect where
  evalSafely act = liftF $ EvalSafelyAsyncEffect act id

instance Awaitable Effect where
  await var = liftF $ Await var id

instance Awaitable AsyncEffect where
  await var = liftF $ Await var id

instance Asynchronous AsyncEffect where
  async asyncEff = liftF $ Async' asyncEff id


instance Effect' Effect Identity where
  evalConsole consoleAct = liftF $ EvalConsole consoleAct id
  evalIO ioEff = liftF $ EvalIOEff (L.evalIO' ioEff) id

instance Effect' AsyncEffect T.Async where
  evalConsole consoleAct = liftF $ EvalConsole consoleAct id
  evalIO ioEff = liftF $ EvalIOEff (L.evalIO' ioEff) id

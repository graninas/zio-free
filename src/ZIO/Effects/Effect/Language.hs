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
  Async :: AsyncEffect a -> (T.Async a -> next) -> EffectF m next
  Await :: T.Async a -> (a -> next) -> EffectF m next

type Effect = Free (EffectF Identity)
type AsyncEffect = Free (EffectF T.Async)

instance Functor (EffectF m) where
  fmap f (EvalConsole consoleAct next) = EvalConsole consoleAct (f . next)
  fmap f (EvalIOEff ioEff next) = EvalIOEff ioEff (f . next)
  fmap f (Async asyncEff next) = Async asyncEff (f . next)
  fmap f (Await var next) = Await var (f . next)

----------------

class Awaitable m where
  await :: T.Async a -> m a

class Asynchronous m where
  async :: AsyncEffect a -> m (T.Async a)

class Awaitable m => Effect' m mode | m -> mode where
  evalConsole :: L.Console a -> m (mode a)
  evalIO :: IO a -> m (mode a)

-----------------

instance Awaitable Effect where
  await var = liftF $ Await var id

instance Awaitable AsyncEffect where
  await var = liftF $ Await var id

instance Asynchronous AsyncEffect where
  async asyncEff = liftF $ Async asyncEff id


instance Effect' Effect Identity where
  evalConsole consoleAct = liftF $ EvalConsole consoleAct id
  evalIO ioEff = liftF $ EvalIOEff (L.evalIO' ioEff) id

instance Effect' AsyncEffect T.Async where
  evalConsole consoleAct = liftF $ EvalConsole consoleAct id
  evalIO ioEff = liftF $ EvalIOEff (L.evalIO' ioEff) id

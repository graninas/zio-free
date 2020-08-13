{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FunctionalDependencies #-}

module ZIO.Effects.Effect.Language where

import           ZIO.Prelude
import qualified Prelude as P
import qualified System.Process as Proc

import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Effects.IO.Language as L
import qualified ZIO.Types as T

data EffectF m next where
  RunConsole :: L.Console a -> (m a -> next) -> EffectF m next
  RunIOEff :: L.IOEff a -> (m a -> next) -> EffectF m next
  Async :: AsyncEffect a -> (T.Async a -> next) -> EffectF m next
  Await :: T.Async a -> (a -> next) -> EffectF m next

type Effect = Free (EffectF Identity)
type AsyncEffect = Free (EffectF T.Async)

instance Functor (EffectF m) where
  fmap f (RunConsole consoleAct next) = RunConsole consoleAct (f . next)
  fmap f (RunIOEff ioEff next) = RunIOEff ioEff (f . next)
  fmap f (Async asyncEff next) = Async asyncEff (f . next)
  fmap f (Await var next) = Await var (f . next)

class Awaitable m where
  await :: T.Async a -> m a

class Asynchronous m where
  async :: AsyncEffect a -> m (T.Async a)

class Awaitable m => Effect' m mode | m -> mode where
  runConsole :: L.Console a -> m (mode a)
  runIO :: IO a -> m (mode a)


instance Awaitable Effect where
  await var = liftF $ Await var id

instance Awaitable AsyncEffect where
  await var = liftF $ Await var id

instance Asynchronous AsyncEffect where
  async asyncEff = liftF $ Async asyncEff id


instance Effect' Effect Identity where
  runConsole consoleAct = liftF $ RunConsole consoleAct id
  runIO ioEff = liftF $ RunIOEff (L.runIO' ioEff) id

instance Effect' AsyncEffect T.Async where
  runConsole consoleAct = liftF $ RunConsole consoleAct id
  runIO ioEff = liftF $ RunIOEff (L.runIO' ioEff) id

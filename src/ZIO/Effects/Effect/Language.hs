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
  RunConsole :: L.Console a -> (m a -> next) -> EffectF m next
  RunIOEff :: L.IOEff a -> (m a -> next) -> EffectF m next

type Effect = Free (EffectF Identity)
type EffectAsync = Free (EffectF T.Async)

instance Functor (EffectF m) where
  fmap f (RunConsole act next) = RunConsole act (f . next)
  fmap f (RunIOEff ioEff next) = RunIOEff ioEff (f . next)

runConsole :: L.Console a -> Effect a
runConsole consoleAct = runIdentity <$> (liftF $ RunConsole consoleAct id)

runIO :: IO a -> Effect a
runIO ioEff = runIdentity <$> (liftF $ RunIOEff (L.runIO' ioEff) id)

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Effects.Effect.Language where

import           ZIO.Prelude
import qualified Prelude as P
import qualified System.Process as Proc

import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Effects.IO.Language as L

data EffectF next where
  RunConsole :: L.Console a -> (a -> next) -> EffectF next
  RunIOEff :: L.IOEff a -> (a -> next) -> EffectF next

type Effect = Free EffectF

instance Functor EffectF where
  fmap f (RunConsole act next) = RunConsole act (f . next)
  fmap f (RunIOEff ioEff next) = RunIOEff ioEff (f . next)

runConsole :: L.Console a -> Effect a
runConsole consoleAct = liftF $ RunConsole consoleAct id

runIO :: IO a -> Effect a
runIO ioEff = liftF $ RunIOEff (L.runIO' ioEff) id

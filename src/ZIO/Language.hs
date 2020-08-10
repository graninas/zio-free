{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Language where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Types as T

data ZIOF next where
  RunAsync :: L.EffectAsync a -> (a -> next) -> ZIOF next
  RunSynchronously :: L.EffectAsync a -> (a -> next) -> ZIOF next
  RunEffect :: L.Effect a -> (a -> next) -> ZIOF next

instance Functor ZIOF  where
  fmap f (RunAsync asyncEff next) = RunAsync asyncEff (f . next)
  fmap f (RunSynchronously asyncEff next) = RunSynchronously asyncEff (f . next)
  fmap f (RunEffect eff next) = RunEffect eff (f . next)

type ZIO = Free ZIOF

runAsync :: L.EffectAsync a -> ZIO a
runAsync eff = liftF $ RunAsync eff id

runSynchronously :: L.EffectAsync a -> ZIO a
runSynchronously eff = liftF $ RunSynchronously eff id

runEffect :: L.Effect a -> ZIO a
runEffect eff = liftF $ RunEffect eff id

instance L.Awaitable ZIO where
  await var = runEffect $ L.await var

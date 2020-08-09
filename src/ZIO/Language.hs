{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Language where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Types as T

data ZIOF next where
  RunAsync :: L.EffectAsync a -> (a -> next) -> ZIOF next
  RunSync :: L.Effect a -> (a -> next) -> ZIOF next
  Await' :: T.Async a -> (a -> next) -> ZIOF next

instance Functor ZIOF  where
  fmap f (RunAsync act next) = RunAsync act (f . next)
  fmap f (RunSync act next) = RunSync act (f . next)
  fmap f (Await' var next) = Await' var (f . next)

type ZIO = Free ZIOF

runAsync :: L.EffectAsync a -> ZIO a
runAsync eff = liftF $ RunAsync eff id

runSync :: L.Effect a -> ZIO a
runSync eff = liftF $ RunSync eff id

instance L.Awaitable ZIO where
  await var = liftF $ Await' var id

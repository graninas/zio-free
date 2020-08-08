{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Language where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Types as T
import           Unsafe.Coerce (unsafeCoerce)

data ZIOF m next where
  RunAsync :: L.EffectAsync a -> (m a -> next) -> ZIOF m next
  RunSync :: L.Effect a -> (m a -> next) -> ZIOF m next

instance Functor (ZIOF m) where
  fmap f (RunAsync act next) = RunAsync act (f . next)
  fmap f (RunSync act next) = RunSync act (f . next)

type ZIO = Free (ZIOF Identity)
type ZIOAsync = Free (ZIOF T.Async)

runAsync :: L.EffectAsync a -> ZIO a
runAsync act = runIdentity <$> (liftF $ RunAsync act id)

runSync :: L.Effect a -> ZIO a
runSync act = runIdentity <$> (liftF $ RunSync act id)

asAsync :: L.Effect a -> L.EffectAsync a
asAsync = unsafeCoerce

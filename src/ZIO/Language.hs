{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Language where

import           ZIO.Prelude

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Types as T

data ZIOF m next where
  RunAsync :: ZIOAsync a -> (m a -> next) -> ZIOF m next
  RunSync :: ZIO a -> (m a -> next) -> ZIOF m next
  RunEffect :: L.Effect a -> (m a -> next) -> ZIOF m next

instance Functor (ZIOF m) where
  fmap f (RunAsync act next) = RunAsync act (f . next)
  fmap f (RunSync act next) = RunSync act (f . next)
  fmap f (RunEffect eff next) = RunEffect eff (f . next)

type ZIO = Free (ZIOF Identity)
type ZIOAsync = Free (ZIOF T.Async)

runEffect :: L.Effect a -> ZIO a
runEffect eff = runIdentity <$> (liftF $ RunEffect eff id)

runAsync :: ZIOAsync a -> ZIO a
runAsync act = runIdentity <$> (liftF $ RunAsync act id)

runSync :: ZIO a -> ZIO a
runSync act = runIdentity <$> (liftF $ RunSync act id)

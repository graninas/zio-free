module ZIO.Types where

import ZIO.Prelude


data Async a
  -- = forall b. Async (b -> a) (MVar (Either SomeException b))
  = forall b. Async (b -> a) (MVar b)
  | Ready a

instance Functor Async where
  fmap f (Async f' mvar) = Async (f . f') mvar
  fmap f (Ready val) = Ready $ f val

module ZIO.Types where

import ZIO.Prelude

data Async a
  = forall b. Async (b -> a) (MVar b)
  | Ready a

instance Functor Async where
  fmap f (Async f' mvar) = Async (f . f') mvar
  fmap f (Ready val) = Ready $ f val



instance Applicative Async where
  pure val = Ready val
  (Ready f) <*> async = fmap f async
  (Async ff mvar) <*> (Ready val) = Async (\b -> ff b val) mvar
  (Async ff mvar1) <*> (Async f mvar2) = Async (\) mvar?

-- f :: a -> c
-- Ready (val :: a)
-- Async (b -> c) (MVar b)
--
--
--
-- mvar :: b
-- ff :: b -> (a -> c)
-- Ready (val :: a)
-- Async c


Async (b1 -> (a -> c)) (MVar b1)

Async (b2 -> a) (MVar b2)

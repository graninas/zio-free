{-# LANGUAGE TypeFamilies #-}

module ZIO.Types where

import           ZIO.Prelude

import qualified Data.Map as Map



data Async a where
  Async :: (b -> a) -> MVar b -> Async a
  Ready :: a -> Async a


instance Functor Async where
  fmap f (Async f' mvar) = Async (f . f') mvar
  fmap f (Ready val) = Ready $ f val

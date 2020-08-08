{-# LANGUAGE TypeFamilies #-}

module ZIO.Types where

import           ZIO.Prelude

import qualified Data.Map                        as Map

newtype Delayed a = Delayed (MVar a)

newtype Async a = Async (MVar a)


-- "Higher-Kinded Data"
-- https://reasonablypolymorphic.com/blog/higher-kinded-data/
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

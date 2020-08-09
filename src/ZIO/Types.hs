{-# LANGUAGE TypeFamilies #-}

module ZIO.Types where

import           ZIO.Prelude

import qualified Data.Map as Map



data Async a = Async (MVar a) | Ready a


-- "Higher-Kinded Data"
-- https://reasonablypolymorphic.com/blog/higher-kinded-data/
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

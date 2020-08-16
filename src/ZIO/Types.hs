{-# LANGUAGE TypeFamilies #-}

module ZIO.Types where

import           ZIO.Prelude

import qualified Data.Map as Map



data Async a
  = Async (MVar a)
  | Ready a

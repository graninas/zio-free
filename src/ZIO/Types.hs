module ZIO.Types where

import ZIO.Prelude

data Async a
  = Async (MVar a)
  | Ready a

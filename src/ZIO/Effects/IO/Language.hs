{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Effects.IO.Language where

import           ZIO.Prelude


data IOF next where
  RunIO :: IO a -> (a -> next) -> IOF next

instance Functor IOF where
  fmap f (RunIO ioAct next) = RunIO ioAct (f . next)

type IOEff = Free IOF

runIO' :: IO a -> IOEff a
runIO' ioAct = liftF $ RunIO ioAct id

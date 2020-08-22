{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Effects.IO.Language where

import           ZIO.Prelude


data IOF next where
  EvalIO :: IO a -> (a -> next) -> IOF next

instance Functor IOF where
  fmap f (EvalIO ioAct next) = EvalIO ioAct (f . next)

type IOEff = Free IOF

evalIO' :: IO a -> IOEff a
evalIO' ioAct = liftF $ EvalIO ioAct id

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Language where

import           ZIO.Prelude


data ZIOF next where
  EvalIO :: IO a -> (a  -> next) -> ZIOF next

instance Functor ZIOF where
  fmap f (EvalIO ioAct next) = EvalIO ioAct (f . next)

type ZIO = Free ZIOF

evalIO :: IO a -> ZIO a
evalIO ioAct = liftF $ EvalIO ioAct id

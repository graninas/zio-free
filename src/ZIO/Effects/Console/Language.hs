{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Effects.Console.Language where

import           ZIO.Prelude


data ConsoleF next where
  PrintLine :: String -> (() -> next) -> ConsoleF next
  ReadLine :: (String -> next) -> ConsoleF next

type Console = Free ConsoleF

instance Functor ConsoleF where
  fmap f (PrintLine line next) = PrintLine line (f . next)
  fmap f (ReadLine next) = ReadLine (f . next)

printLine :: String -> Console ()
printLine line = liftF $ PrintLine line id

readLine :: Console String
readLine = liftF $ ReadLine id

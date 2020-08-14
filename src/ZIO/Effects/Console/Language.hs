{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Effects.Console.Language where

import           ZIO.Prelude hiding (putStrLn, getLine)
import qualified Prelude as P

data ConsoleF next where
  PutStrLn :: String -> (() -> next) -> ConsoleF next
  GetStrLn :: (String -> next) -> ConsoleF next

type Console = Free ConsoleF

instance Functor ConsoleF where
  fmap f (PutStrLn line next) = PutStrLn line (f . next)
  fmap f (GetStrLn next) = GetStrLn (f . next)


getStrLn :: Console String
getStrLn = liftF $ GetStrLn id


putStrLn :: String -> Console ()
putStrLn line = liftF $ PutStrLn line id

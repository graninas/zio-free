{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.Console.Interpreter where

import           ZIO.Prelude
import qualified Prelude as P

import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Runtime as R

interpretConsoleF :: R.ZIORuntime -> L.ConsoleF a -> IO a
interpretConsoleF _ (L.PrintLine line next) =
  next <$> P.putStrLn line

interpretConsoleF _ (L.ReadLine next) = do
  !line <- P.getLine
  pure $ next line

runConsole :: R.ZIORuntime -> L.Console a -> IO a
runConsole rt = foldFree (interpretConsoleF rt)

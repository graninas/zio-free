{-# LANGUAGE BangPatterns #-}

module ZIO.Effects.Console.Interpreter where

import           ZIO.Prelude
import qualified Prelude as P

import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Runtime as R

interpretConsoleF :: R.ZIORuntime -> L.ConsoleF a -> IO a
interpretConsoleF _ (L.PutStrLn line next) =
  next <$> P.putStrLn line

interpretConsoleF _ (L.GetStrLn next) = do
  !line <- P.getLine
  pure $ next line

runConsole :: R.ZIORuntime -> L.Console a -> IO a
runConsole rt (Pure val) = pure val
runConsole rt (Free act) = do
  next <- interpretConsoleF rt act
  runConsole rt next

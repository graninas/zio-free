module Main where

import qualified Prelude as P
import qualified System.Process as Proc
import           ZIO.Prelude

import qualified ZIO.Runtime as R
import qualified ZIO.Interpreter as R
import ZIO.Language as L
import ZIO.Effects.IO.Language as L
import ZIO.Effects.Console.Language as L
import ZIO.Effects.Effect.Language as L

fibonacci :: Int -> Int
fibonacci n = head $ drop n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factorial :: Int -> Int
factorial n = product [1..n]

app :: ZIO (Int, Int)
app = do
  fib <- runEffect $ runIO $ do
    P.putStrLn "Fib started"
    threadDelay $ 1000 * 1000
    let fib = fibonacci 20
    P.putStrLn $ "Fib: " <> show fib
    pure fib

  fact <- runEffect $ runIO $ do
    P.putStrLn "Fact started"
    let fact = factorial 20
    P.putStrLn $ "Fact: " <> show fact
    pure fact

  pure (fib, fact)


main :: IO ()
main = do
  rt <- R.createZIORuntime
  (fib, fact) <- R.runZIOSync rt app
  -- asyncVar <- R.runZIOAsync rt app
  -- (fib, fact) <- R.awaitAsyncVar asyncVar
  P.putStrLn $ "Fib + Fact: " <> show (fib + fact)

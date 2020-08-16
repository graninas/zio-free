module Main where

import qualified Prelude as P
import           ZIO.Prelude hiding (putStrLn, getLine)

import qualified ZIO.Runtime as R
import qualified ZIO.Interpreter as R
import qualified ZIO.Language as L
import qualified ZIO.Effects.Effect.Language as L

fibonacci :: Int -> Int
fibonacci n = head $ drop n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factorial :: Int -> Int
factorial n = product [1..n]

fibIO :: IO Int
fibIO = do
  P.putStrLn "Fib started"
  threadDelay $ 1000 * 1000 * 2
  let fib = fibonacci 20
  P.putStrLn $ "Fib: " <> show fib
  pure fib

factIO :: IO Int
factIO = do
  P.putStrLn "Fact started"
  let fact = factorial 20
  P.putStrLn $ "Fact: " <> show fact
  pure fact

asyncApp :: L.AsyncEffect (Int, Int)
asyncApp = do
  aFib  <- L.runIO fibIO
  aFact <- L.runIO factIO
  fib   <- L.await aFib
  fact  <- L.await aFact
  pure (fib, fact)

app :: L.ZIO (Int, Int)
app = L.runAsyncEffect asyncApp

main :: IO ()
main = R.withZIORuntime $ \rt -> do
  (fib, fact) <- R.runZIO rt app
  print "You got:"
  print (fib, fact)

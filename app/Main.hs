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

fibonacci :: Integer -> Integer
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factorial :: Integer -> Integer
factorial n = product [1..n]

app :: ZIO (Integer, Integer)
app = do
  line <- runEffect $ runIO P.getLine

  fib <- runEffect $ runIO $ do
    P.putStrLn "Action 1 started."
    threadDelay $ 1000 * 1000
    let fib = fibonacci 20
    P.putStrLn "Action 1 finished."
    pure fib

  fact <- runEffect $ runIO $ do
    P.putStrLn "Action 2 started."
    let fact = factorial 20
    P.putStrLn "Action 2 finished."
    pure fact

  pure (fib, fact)


main :: IO ()
main = do
  rt <- R.createZIORuntime
  v1 <- R.runZIOSync rt app
  asyncVar <- R.runZIOAsync rt app
  (fib, fact) <- R.awaitAsyncVar asyncVar
  P.putStrLn $ "Fib: " <> show fib
  P.putStrLn $ "Fact: " <> show fact

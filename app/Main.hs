module Main where

import qualified Prelude as P
import qualified System.Process as Proc
import           ZIO.Prelude hiding (putStrLn, getLine)

import qualified ZIO.Runtime as R
import qualified ZIO.Interpreter as R
import ZIO.Language as L
import ZIO.Effects.IO.Language as L

fibonacci :: Int -> Int
fibonacci n = head $ drop n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factorial :: Int -> Int
factorial n = product [1..n]

fibApp :: ZIO Int
fibApp = runIO $ do
  P.putStrLn "Fib started"
  threadDelay $ 1000 * 1000 * 2
  let fib = fibonacci 20
  P.putStrLn $ "Fib: " <> show fib
  pure fib

factApp :: ZIO Int
factApp = runIO $ do
  P.putStrLn "Fact started"
  let fact = factorial 20
  P.putStrLn $ "Fact: " <> show fact
  pure fact

app :: ZIO (Int, Int)
app = do
  fib  <- fibApp
  fact <- factApp
  pure (fib, fact)


main :: IO ()
main = R.withZIORuntime $ \rt -> do
  -- (fib, fact) <- R.runZIO rt app
  var <- R.runZIOAsync rt app
  (fib, fact) <- takeMVar var

  print "You got:"
  print (fib, fact)

  -- asyncVar <- R.runZIOAsync rt app
  -- (fib, fact) <- R.awaitAsyncVar asyncVar


--
-- main :: IO ()
-- main = R.withZIORuntime $
--   \rt -> R.runZIO rt myApp
--
-- myApp :: ZIO ()
-- myApp = do
--   L.putStrLn "Hello! What is your name?"
--   name <- L.getStrLn
--   L.putStrLn ("Hello, " <> name <> ", welcome to ZIO!")
--   pure ()

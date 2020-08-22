module Main where

import qualified Prelude as P
import           ZIO.Prelude hiding (putStrLn, getLine)
import qualified System.Process as Proc

import qualified ZIO.Types as T
import qualified ZIO.Runtime as R
import qualified ZIO.Interpreter as R
import qualified ZIO.Language as L
import qualified ZIO.Effects.Effect.Language as L


wgetGist :: String -> L.AsyncEffect (T.Async String)
wgetGist gist = L.evalIO $ do
  Proc.readCreateProcess (Proc.shell ("wget https://gist.github.com/graninas/" <> gist)) ""
  P.readFile gist


gistLength :: L.AsyncEffect (Int, Int)
gistLength = do
  aGist :: T.Async String <- wgetGist "01565065c18c01e88a5ebcbfbb96e397"

  gist1  :: String <- L.await aGist
  gist2  :: String <- L.await aGist
  gist3  :: String <- L.await aGist

  pure (length gist1, length $ gist1 <> gist2 <> gist3)


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
  aFib  <- L.evalIO fibIO
  aFact <- L.evalIO factIO
  fib   <- L.await aFib
  fact  <- L.await aFact
  pure (fib, fact)

app :: L.ZIO (Int, Int)
app = L.evalAsyncEffect asyncApp

main :: IO ()
main = R.withZIORuntime $ \rt -> do
  -- (fib, fact) <- R.runZIO rt app
  -- print "You got:"
  -- print (fib, fact)

  v <- R.runZIO rt $ L.evalAsyncEffect gistLength
  print "You got:"
  print v

module Main where

import qualified Prelude as P
import           ZIO.Prelude hiding (putStrLn, getLine)
import qualified System.Process as Proc
import           Control.Exception (throwIO)

import qualified ZIO.Types as T
import qualified ZIO.Runtime as R
import qualified ZIO.Interpreter as R
import qualified ZIO.Language as L
import qualified ZIO.Effects.Effect.Language as L


data Dummy = Dummy deriving (Show, Typeable)
instance Exception Dummy

wgetGistIO :: String -> IO String
wgetGistIO gist = do
  Proc.readCreateProcess (Proc.shell ("wget https://gist.github.com/graninas/" <> gist)) ""
  P.readFile gist

wgetGistIO' :: String -> IO (Either SomeException String)
wgetGistIO' gist = do
  Proc.readCreateProcess (Proc.shell ("wget https://gist.github.com/graninas/" <> gist)) ""
  try $ P.readFile gist

wgetGist :: String -> L.AsyncEffect (T.Async String)
wgetGist gist = L.evalIO $ wgetGistIO gist

wgetGistBroken :: String -> L.AsyncEffect (T.Async String)
wgetGistBroken gist = L.evalIO $ do
  wgetGistIO gist
  throwIO Dummy

wgetGist' :: String -> L.AsyncEffect (T.Async (Either SomeException String))
wgetGist' gist = L.evalIO $ wgetGistIO' gist

-- Sample 1


appLogic :: L.AsyncEffect ()
appLogic = do
  line' :: T.Async String <- L.evalIO $ pure "App methods finished."

  eRes1' :: T.Async (Either SomeException String) <- L.evalIO $ do
    threadDelay $ 1000 * 1000 * 2
    P.putStrLn "Downloading 1..."
    void $ wgetGistIO' "01565065c18c01e88a5ebcbfbb96e397"
    P.putStrLn "Finished downloading 1."
    threadDelay $ 1000 * 1000
    content <- try $ P.readFile "01565065c18c01e88a5ebcbfbb96e397"
    P.putStrLn "File 1 read."
    pure content

  eRes2' :: T.Async (Either SomeException String) <- L.evalIO $ do
    threadDelay $ 1000 * 500 * 2
    P.putStrLn "Downloading 2..."
    void $ wgetGistIO' "c7e0a603f3a22c7e85daa4599bf92525"
    P.putStrLn "Finished downloading 2."
    threadDelay $ 1000 * 500
    content <- try $ P.readFile "c7e0a603f3a22c7e85daa4599bf92525"
    P.putStrLn "File 2 read."
    pure content

  L.evalIO $ P.putStrLn "Awaiting line'..."
  line :: String <- L.await line'

  result' <- L.async $ do
    _ <- L.await eRes1'
    eRes2 <- L.await eRes2'

    let resLine1' = (either show id) <$> eRes1'
    let resLine2 = either show id eRes2

    resLine1 <- L.await resLine1'

    pure (line <> " " <> resLine1 <> " " <> resLine2)

  L.evalIO $ P.putStrLn "Some async event."

  result <- L.await result'

  L.evalIO $ P.putStrLn "All data got."
  L.evalIO $ P.putStrLn result
  pure ()

sampleApp1 :: L.ZIO ()
sampleApp1 = void $ L.evalAsyncEffect appLogic

-- Sample 2

gistLengthUnsafe :: L.AsyncEffect (Int, Int)
gistLengthUnsafe = do
  aGist :: T.Async String <- wgetGistBroken "01565065c18c01e88a5ebcbfbb96e397"
  let aLen = length <$> aGist
  len :: Int <- L.await aLen
  pure (len, len * 2)

gistLengthSafe :: L.ZIO ()
gistLengthSafe = do
  eVal <- L.evalSafely $ L.evalAsyncEffect gistLengthUnsafe
  void $ case eVal of
    Left (err :: SomeException)
      -> L.evalIO $ P.putStrLn $ "Exception got: " <> show err
    Right val
      -> L.evalIO $ P.putStrLn $ "You got: " <> show val

sampleApp2 :: L.ZIO ()
sampleApp2 = gistLengthSafe


-- Sample 3

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

sampleApp3 :: L.ZIO ()
sampleApp3 = do
  fibAndFact <- L.evalAsyncEffect asyncApp
  void $ L.evalIO $ print fibAndFact

-- Sample 4

sampleApp4 :: L.ZIO ()
sampleApp4 = L.evalAsyncEffect $ do
  vars <- mapM L.evalIO
    [ print 1
    , threadDelay (1000 * 1000 * 2) >> print 2
    , print 3
    , threadDelay (1000 * 1000 * 3) >> print 4
    , print 5
    , threadDelay (1000 * 1000 * 1) >> print 6
    , threadDelay (1000 * 1000 * 4) >> print 7
    , print 8
    , print 9
    , threadDelay (1000 * 1000 * 3) >> print 10
    , print 11
    , print 12
    ]
  mapM_ L.await vars



main :: IO ()
main = R.withZIORuntime $ \rt ->
  R.runZIO rt sampleApp4

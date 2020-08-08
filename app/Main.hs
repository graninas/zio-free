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


app :: ZIO Int
app = do
  line <- runAsync $ asAsync $ runIO $ pure "App methods finished."

  eRes1 <- runAsync $ asAsync $ runIO $ do
    threadDelay $ 1000 * 1000
    P.putStrLn "Downloading 1..."
    _ :: Either SomeException String <- try $ Proc.readCreateProcess (Proc.shell "wget https://gist.github.com/graninas/01565065c18c01e88a5ebcbfbb96e397") ""
    P.putStrLn "Finished downloading 1."
    threadDelay $ 1000 * 500
    try $ P.readFile "01565065c18c01e88a5ebcbfbb96e397"

  eRes2 <- runAsync $ asAsync $ runIO $ do
    threadDelay $ 1000 * 500
    P.putStrLn "Downloading 2..."
    _ :: Either SomeException String <- try $ Proc.readCreateProcess (Proc.shell "wget https://gist.github.com/graninas/c7e0a603f3a22c7e85daa4599bf92525") ""
    P.putStrLn "Finished downloading 2."
    threadDelay $ 1000 * 1000
    try $ P.readFile "c7e0a603f3a22c7e85daa4599bf92525"

  runAsync $ asAsync $ runIO $ do
    P.putStrLn line
    let (content1, content2, size) = case (eRes1, eRes2) of
          (Left (err1 :: SomeException), _) -> (show err1, "", 0)
          (_, Left (err2 :: SomeException)) -> ("", show err2, 0)
          (Right content1, Right content2) -> (content1, content2, length content1 + length content2)
    -- P.putStrLn content1
    -- P.putStrLn content2
    P.putStrLn $ show size
    pure size


main :: IO ()
main = do
  rt <- R.createZIORuntime
  v1 <- R.runZIO rt app
  P.putStrLn $ show v1
  -- R.Async var2 <- R.runZIO rt app
  -- v2 <- takeMVar var2
  -- P.putStrLn $ show v2

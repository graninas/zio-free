module Main where

import qualified Prelude as P
import qualified System.Process as Proc
import           ZIO.Prelude

import qualified ZIO.Runtime as R
import qualified ZIO.Interpreter as R
import qualified ZIO.Types as T
import ZIO.Language as L
import ZIO.Effects.IO.Language as L
import ZIO.Effects.Console.Language as L
import ZIO.Effects.Effect.Language as L


appLogic :: L.AsyncEffect String
appLogic = do
  line' :: T.Async String <- runIO $ pure "App methods finished."

  eRes1' :: T.Async (Either SomeException String) <- runIO $ do
    threadDelay $ 1000 * 1000 * 2
    P.putStrLn "Downloading 1..."
    _ :: Either SomeException String <- try $ Proc.readCreateProcess (Proc.shell "wget https://gist.github.com/graninas/01565065c18c01e88a5ebcbfbb96e397") ""
    P.putStrLn "Finished downloading 1."
    threadDelay $ 1000 * 1000
    content <- try $ P.readFile "01565065c18c01e88a5ebcbfbb96e397"
    P.putStrLn "File 1 read."
    pure content

  eRes2' :: T.Async (Either SomeException String) <- runIO $ do
    threadDelay $ 1000 * 500 * 2
    P.putStrLn "Downloading 2..."
    _ :: Either SomeException String <- try $ Proc.readCreateProcess (Proc.shell "wget https://gist.github.com/graninas/c7e0a603f3a22c7e85daa4599bf92525") ""
    P.putStrLn "Finished downloading 2."
    threadDelay $ 1000 * 500
    content <- try $ P.readFile "c7e0a603f3a22c7e85daa4599bf92525"
    P.putStrLn "File 2 read."
    pure content

  runIO $ P.putStrLn "Awaiting line'..."
  line :: String <- await line'

  result' <- async $ do
    runIO $ P.putStrLn "Awaiting eRes1'..."
    eRes1 <- await eRes1'

    runIO $ P.putStrLn "Some IO delay here..."
    runIO $ threadDelay $ 1000 * 1000 * 3
    runIO $ P.putStrLn "IO delay finished."

    runIO $ P.putStrLn "Awaiting eRes2'..."
    eRes2 <- await eRes2'

    resLine1' :: T.Async String <- runIO $ case eRes1 of
        Left err1 -> pure $ show err1
        Right (content1 :: String) -> pure content1

    let resLine2 = case eRes2 of
          Left err2 -> show err2
          Right (content2 :: String) -> content2

    runIO $ P.putStrLn "Awaiting resLine1'..."
    resLine1 <- await resLine1'

    pure (line <> " " <> resLine1 <> " " <> resLine2)

  runIO $ P.putStrLn "Some async event."

  result <- await result'

  runIO $ P.putStrLn "All data got."
  pure result


app :: ZIO String
app = do
  v1 <- runAsyncEffect appLogic
  -- v1 <- runSynchronously appLogic
  pure v1

main :: IO ()
main = do
  rt <- R.createZIORuntime
  v1 <- R.runZIO rt app

  void $ P.getLine

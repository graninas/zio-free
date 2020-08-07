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
  line <- runEffect $ runIO P.getLine

  eRes <- runEffect $ runIO $ do
    _ :: Either SomeException String <- try $ Proc.readCreateProcess (Proc.shell "wget https://gist.github.com/graninas/01565065c18c01e88a5ebcbfbb96e397") ""
    try $ P.readFile "01565065c18c01e88a5ebcbfbb96e397"

  runEffect $ runIO $ do
    P.putStrLn line
    let (content, size) = case eRes of
          Left (err :: SomeException) -> (show err, 0)
          Right content -> (content, length content)
    P.putStrLn content
    P.putStrLn $ show size
    pure size


main :: IO ()
main = do
  rt <- R.createZIORuntime
  v1 <- R.runZIOSync rt app
  R.Delayed var2 <- R.runZIOAsync rt app
  v2 <- takeMVar var2
  P.putStrLn $ show $ v1 + v2

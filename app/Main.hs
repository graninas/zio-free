module Main where

import qualified Prelude as P
import qualified System.Process as Proc
import           ZIO.Prelude hiding (try)
import Control.Exception
import Data.Typeable

import qualified ZIO.Runtime as R
import qualified ZIO.Interpreter as R
import qualified ZIO.Types as T
import ZIO.Language as L
import ZIO.Effects.IO.Language as L
import ZIO.Effects.Console.Language as L
import ZIO.Effects.Effect.Language as L

-- Example 1. "BadRace"

badRace :: ZIO a -> ZIO b -> ZIO (Either a b)
badRace zioa ziob = do
  -- TODO: state, processes
  -- mvar <- newEmptyMVar
  -- tida <- forkIO $ zioa >>= putMVar mvar . Left
  -- tidb <- forkIO $ ziob >>= putMVar mvar . Right
  -- res <- takeMVar mvar
  -- killThread tida
  -- killThread tidb
  -- return res
  error "Not implemented yet."

zioInt :: ZIO Int
zioInt = do
  -- some code here
  pure 10

zioStr :: ZIO String
zioStr = do
  -- some code here
  pure "ABC"

badRaceApp :: ZIO ()
badRaceApp = do
  eRes <- badRace zioInt zioStr
  case eRes of
    Left intVal  -> runIO $ P.putStrLn $ "Int value got: " <> show intVal
    Right strVal -> runIO $ P.putStrLn $ "Int value got: " <> strVal

-- Example 2. "Subtle Dummy exception"

data Dummy = Dummy
  deriving (Show, Typeable)
instance Exception Dummy

zioPrinter :: ZIO (Either Dummy ()) -> ZIO ()
zioPrinter x = x >>= L.runIO . print

dummyPrinterApp :: ZIO ()
dummyPrinterApp = do
  L.runIO $ P.putStrLn "runIO, native throw functions, zio catch"

  zioPrinter $ L.runSafely $ L.runIO $ throwIO Dummy             -- prints "Left Dummy"
  zioPrinter $ L.runSafely $ L.runIO $ throw Dummy               -- prints "Left Dummy"
  zioPrinter $ L.runSafely $ L.runIO $ evaluate $ throw Dummy    -- prints "Left Dummy"
  zioPrinter $ L.runSafely $ L.runIO $ return $! throw Dummy     -- prints "Left Dummy"
  zioPrinter $ L.runSafely $ L.runIO $ return $ throw Dummy      -- prints "Left Dummy"

  L.runIO $ P.putStrLn "runIO, zio throw, zio catch"

  zioPrinter $ L.runSafely $ L.throwException Dummy
  -- zioPrinter $ L.runSafely $ try $ throw Dummy                  -- can't be compiled
  -- zioPrinter $ L.runSafely $ evaluate $ L.throwException Dummy  -- can't be compiled
  -- zioPrinter $ L.runSafely $ return $! L.throwException Dummy   -- can't be compiled
  -- zioPrinter $ L.runSafely $ return $ L.throwException Dummy    -- can't be compiled

  L.runIO $ P.putStrLn "runIO, native throw functions, native catch"

  zioPrinter $ L.runIO $ try $ throwIO Dummy             -- prints "Left Dummy"
  zioPrinter $ L.runIO $ try $ throw Dummy               -- prints "Left Dummy"
  zioPrinter $ L.runIO $ try $ evaluate $ throw Dummy    -- prints "Left Dummy"
  zioPrinter $ L.runIO $ try $ return $! throw Dummy     -- prints "Left Dummy"
  zioPrinter $ L.runIO $ try $ return $ throw Dummy      -- throws an exception. It will leak outside.

safeDummyPrinterApp :: ZIO ()
safeDummyPrinterApp = do
  eRes <- L.runSafely dummyPrinterApp
  case eRes of
    Left (err :: SomeException) -> L.runIO $ P.putStrLn $ "Exception thrown: " <> show err
    Right () -> pure ()

main :: IO ()
main = do
  rt <- R.createZIORuntime
  R.runZIO rt safeDummyPrinterApp

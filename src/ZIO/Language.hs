{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Language where

import qualified Prelude as P
import           ZIO.Prelude hiding (putStrLn, getLine)

import qualified ZIO.Effects.Effect.Language as L
import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Effects.IO.Language as L

data ZIOF next where
  RunAsync :: ZIO a -> (a -> next) -> ZIOF next
  RunSync :: ZIO a -> (a -> next) -> ZIOF next
  RunEffect :: L.Effect a -> (a -> next) -> ZIOF next

instance Functor ZIOF where
  fmap f (RunAsync act next) = RunAsync act (f . next)
  fmap f (RunSync act next) = RunSync act (f . next)
  fmap f (RunEffect eff next) = RunEffect eff (f . next)

type ZIO = Free ZIOF

runEffect :: L.Effect a -> ZIO a
runEffect eff = liftF $ RunEffect eff id

runAsync :: ZIO a -> ZIO a
runAsync act = liftF $ RunAsync act id

runSync :: ZIO a -> ZIO a
runSync act = liftF $ RunSync act id

putStrLn :: String -> ZIO ()
putStrLn line = runEffect $ L.runConsole $ L.putStrLn line

getStrLn :: ZIO String
getStrLn = runEffect $ L.runConsole $ L.getStrLn

runIO :: IO a -> ZIO a
runIO ioEff = runEffect $ L.runIO ioEff

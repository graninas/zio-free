{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Effects.Effect.Language where

import           ZIO.Prelude
import qualified Prelude as P
import           Control.Exception (throwIO)

import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Effects.IO.Language as L
import qualified ZIO.Types as T



{- -------------------------------------------------------------

Design 1

-- AsyncEffect lang
-- No need, can be implemented via sync, async & Effect
-- evalConsole :: Console a -> AsyncEffect (T.Async a)     -- catches exceptions, encloses into Async
-- evalIO :: IO a -> AsyncEffect (T.Async a)               -- catches exceptions, encloses into Async

async :: AsyncEffect a -> AsyncEffect (T.Async a)       -- catches exceptions, encloses into Async
await :: T.Async a -> AsyncEffect a                     -- can throw

evalSafely :: AsyncEffect a -> AsyncEffect (Either SomeException a)  -- catches exceptions

sync :: Effect a -> AsyncEffect a                          -- Effect batch works as sync,
                                                           -- operation is blocking (not async)
                                                           -- can throw

    -- AsyncEffect batch works as sync,
    -- operation is blocking (not async)
    -- can throw
asSync :: AsyncEffect a -> AsyncEffect a



-- Effect lang
evalConsole :: Console a -> Effect a                       -- can throw
evalIO :: IO a -> Effect a                                 -- can throw

-- impossible
-- evalAsyncEffect :: AsyncEffect a -> Effect a

evalSafely :: Effect a -> Effect (Either SomeException a)  -- catches exceptions


Note:

evalConsole :: Console a -> AsyncEffect (T.Async a)
evalConsole act = async $ sync $ evalConsole act

evalIO :: IO a -> AsyncEffect (T.Async a)
evalIO act = async $ sync $ evalIO act

-----------------------



-- simple monadic chain

f :: AsyncEffect a
f = do
    am :: Async m <- evalIO ...     -- async action
    an :: Async n <- evalIO ...     -- async action
    m :: m <- await am
    n :: n <- await an
    pure $ use m n
  where
    use :: m -> n -> a
    use = undefined

g :: AsyncEffect b
g = do
    a <- f                          -- sequential chaining
    _ <- f                          -- sequential chaining
    av :: Async v <- evalIO ...     -- async action (but after 2 sequential actions)
    pure $ use a
  where
    use :: a -> b
    use = undefined

-----------------

-- error handling

f :: AsyncEffect a
f = do
    am :: Async m <- evalIO ...     -- throws & encloses E1
    an :: Async n <- evalIO ...     -- throws & encloses E2
    m :: m <- await am              -- throws E1
    n :: n <- await an              -- no evaluation
    pure $ use m n                  -- no evaluation
  where
    use :: m -> n -> a
    use = undefined

g :: AsyncEffect b
g = do
    a <- f    -- sequential chaining    -- throws E1
    _ <- f    -- sequential chaining    -- no evaluation
    av :: Async v <- evalIO ...         -- no evaluation
    pure $ use a                        -- no evaluation
  where
    use :: a -> b
    use = undefined

g' :: AsyncEffect b
g' = do
    aa :: Async a <- async f            -- async evaluation, throws & encloses E1
    _  :: Async a <- async f            -- async evaluation, throws & encloses E1
    av :: Async v <- evalIO ...         -- async evaluation
    pure $ use a                        -- async evaluation
  where
    use :: a -> b
    use = undefined

g'' :: AsyncEffect b
g'' = do
    aa :: Async a <- async f            -- async evaluation, throws & encloses E1
    ea <- evalSafely $ await aa         -- throws, catches
    use ea
  where
    use :: Either SomeException a -> AsyncEffect b
    use = undefined

g''' :: AsyncEffect b
g''' = do
    aa :: Async a <- async f                     -- async evaluation, throws & encloses E1
    aea :: Async (Either SomeException a)
        <- async $ evalSafely $ await aa         -- async evaluation, throws, catches
    ea <- await aea                              -- sync evaluation, no throw
    use ea
  where
    use :: Either SomeException a -> AsyncEffect b
    use = undefined

g'''' :: AsyncEffect b
g'''' = do
    aa :: Async a <- async f                     -- async evaluation, throws & encloses E1
    aa' :: Async a <- async $ await aa           -- async evaluation, throws (on await) & encloses E1
    a :: a <- await aa'                          -- sync evaluation, throws E1
    use a                                        -- no evaluation
  where
    use :: a -> AsyncEffect b
    use = undefined

h :: Effect ()
h = do
    b :: b <- evalAsyncEffect g         -- throws E1
    use b                               -- no evaluation
  where
    use :: b -> AsyncEffect ()
    use = undefined

j :: Effect ()
j = do
    eb :: Either SomeException b <- evalSafely $ evalAsyncEffect g
    use eb
  where
    use :: Either SomeException b -> AsyncEffect ()
    use = undefined


-------------------------------------------------------------

Design 2 (ops are sync by default (return just a))

evalConsole :: Console a -> Effect a
evalIO :: IO a -> Effect a

async :: Effect a -> AsyncEffect (T.Async a)       -- catches exceptions, encloses into Async
async :: AsyncEffect a -> AsyncEffect (T.Async a)  -- catches exceptions, encloses into Async


f :: AsyncEffect a
f = do
    a :: m <- evalIO ...                  -- sync action
    a :: n <- evalIO ...                  -- sync action
    am :: Async m <- async evalIO ...     -- async action
    an :: Async n <- async evalIO ...     -- async action
    m :: m <- await am
    n :: n <- await an
    pure $ use m n
  where
    use :: m -> n -> a
    use = undefined

------------------------------------------------------------- -}


-- Design 1 is implemented here.

data EffectF next where
  -- | Both operations can throw
  EvalConsole :: L.Console a -> (Identity a -> next) -> EffectF next
  EvalIOEff   :: L.IOEff a -> (Identity a -> next) -> EffectF next

  -- impossible
  -- EvalAsyncEffect :: AsyncEffect a -> (a -> next) -> Effect next

  -- | Catches exceptions
  EvalEffectSafely
    :: forall e a next
     . Exception e
    => Effect a -> (Either e a -> next) -> EffectF next

instance Functor EffectF where
  fmap f (EvalConsole cAct next) = EvalConsole cAct (f . next)
  fmap f (EvalIOEff ioEff next) = EvalIOEff ioEff (f . next)
  fmap f (EvalEffectSafely act next) = EvalEffectSafely act (f . next)

type Effect = Free EffectF


data AsyncEffectF next where
  -- | Effect batch works as sync, operation is blocking (not async). Can throw
  EvalEffect :: Effect a -> (a -> next) -> AsyncEffectF next

  -- | Evals async effect asynchronously, catches exceptions, encloses into Async.
  -- 'async' operation.
  Async :: AsyncEffect a -> (T.Async a -> next) -> AsyncEffectF next

  -- | Await for a variable (blocking operation).
  --   Can throw if Async has Exception.
  Await :: T.Async a -> (a -> next) -> AsyncEffectF next

  -- | Catches exceptions
  EvalAsyncEffectSafely
    :: forall e a next. Exception e
    => AsyncEffect a -> (Either e a -> next) -> AsyncEffectF next

type AsyncEffect = Free AsyncEffectF

instance Functor AsyncEffectF where
  fmap f (EvalEffect eff next) = EvalEffect eff (f . next)
  fmap f (Async asyncEff next) = Async asyncEff (f . next)
  fmap f (Await asyncVar next) = Await asyncVar (f . next)
  fmap f (EvalAsyncEffectSafely asyncEff next) = EvalAsyncEffectSafely asyncEff (f . next)

----------------

class Awaiting m where
  await :: T.Async a -> m a

class Effect' m mode | m -> mode where
  evalConsole :: L.Console a -> m (mode a)
  evalIO      :: IO a -> m (mode a)

class EffectEvaluating m where
  evalEffect :: Effect a -> m a

class Throw m where
  throwException :: forall a e. Exception e => e -> m a

class Safe m where
  -- | Catches only a specified type of exceptions or exceptions which are wider.
  -- For example, when SomeException is specified, any exceptions will be catched.
  -- Otherwise depends on the hierarchy of the exceptions.
  evalSafely :: Exception e => m a -> m (Either e a)

-- | Catches any type of exceptions and returns as SomeException.
evalSafely' :: Safe m => m a -> m (Either SomeException a)
evalSafely' = evalSafely

-------------------------

async :: AsyncEffect a -> AsyncEffect (T.Async a)
async asyncEff = liftF $ Async asyncEff id

instance EffectEvaluating AsyncEffect where
  evalEffect eff = liftF $ EvalEffect eff id

instance Awaiting AsyncEffect where
  await asyncVar = liftF $ Await asyncVar id

instance Effect' Effect Identity where
  evalConsole consoleAct = liftF $ EvalConsole consoleAct id
  evalIO ioAct           = liftF $ EvalIOEff (L.evalIO' ioAct) id

instance Effect' AsyncEffect T.Async where
  evalConsole consoleAct = async $ evalEffect (runIdentity <$> evalConsole consoleAct)
  evalIO ioAct           = async $ evalEffect (runIdentity <$> evalIO ioAct)

instance Throw Effect where
  throwException ex = runIdentity <$> (evalIO $ throwIO ex)

instance Throw AsyncEffect where
  throwException ex = evalEffect $ throwException ex

--
-- instance Safe Effect where
--   evalSafely eff = liftF $ EvalEffectSafely eff id
--
-- instance Safe AsyncEffect where
--   evalSafely asyncEff = liftF $ EvalAsyncEffectSafely asyncEff id

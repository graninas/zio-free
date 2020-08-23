{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZIO.Effects.Effect.Language where

import           ZIO.Prelude
import qualified Prelude as P
import qualified System.Process as Proc

import qualified ZIO.Effects.Console.Language as L
import qualified ZIO.Effects.IO.Language as L
import qualified ZIO.Types as T

data EffectF m next where
  EvalConsole :: L.Console a -> (m a -> next) -> EffectF m next
  EvalIOEff :: L.IOEff a -> (m a -> next) -> EffectF m next
  EvalAsyncEffectInAsync :: AsyncEffect a -> (T.Async a -> next) -> EffectF m next
  EvalAsyncEffectInSync :: AsyncEffect a -> (a -> next) -> EffectF m next

  Await :: T.Async a -> (a -> next) -> EffectF m next

  ThrowExceptionEff :: forall m a e next. Exception e
    => e
    -> (a -> next)
    -> EffectF m next

  -- EvalSafelyAsyncEffect :: forall m a next
  --   . AsyncEffect a
  --  -> (T.Async a -> next)
  --  -> EffectF m next
  --
  -- EvalSafelyEffect :: forall m a e next. Exception e
  --  => Effect a
  --  -> (Either e a -> next)
  --  -> EffectF m next

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

type Effect = Free (EffectF Identity)
type AsyncEffect = Free (EffectF T.Async)

instance Functor (EffectF m) where
  fmap f (EvalConsole consoleAct next) = EvalConsole consoleAct (f . next)
  fmap f (EvalIOEff ioEff next) = EvalIOEff ioEff (f . next)
  fmap f (EvalAsyncEffectInAsync asyncEff next) = EvalAsyncEffectInAsync asyncEff (f . next)
  fmap f (EvalAsyncEffectInSync asyncEff next) = EvalAsyncEffectInSync asyncEff (f . next)
  fmap f (Await var next) = Await var (f . next)
  fmap f (ThrowExceptionEff exc next) = ThrowExceptionEff exc (f . next)
  -- fmap f (EvalSafelyAsyncEffect act next) = EvalSafelyAsyncEffect act (f . next)
  -- fmap f (EvalSafelyEffect act next) = EvalSafelyEffect act (f . next)

----------------

class Awaitable m where
  await :: T.Async a -> m a

class AsyncEvaluable m where
  async :: AsyncEffect a -> m a

class Awaitable m => Effect' m mode | m -> mode where
  evalConsole :: L.Console a -> m (mode a)
  evalIO :: IO a -> m (mode a)

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

instance Throw Effect where
  throwException ex = liftF $ ThrowExceptionEff ex id

instance Throw AsyncEffect where
  throwException ex = liftF $ ThrowExceptionEff ex id

-- instance Safe Effect where
--   evalSafely act = liftF $ EvalSafelyEffect act id
--
-- instance Safe AsyncEffect where
--   evalSafely act = liftF $ EvalSafelyAsyncEffect act id

instance Awaitable Effect where
  await var = liftF $ Await var id

instance Awaitable AsyncEffect where
  await var = liftF $ Await var id

instance AsyncEvaluable AsyncEffect where
  async asyncEff = liftF $ EvalAsyncEffectInAsync asyncEff id

instance AsyncEvaluable Effect where
  async asyncEff = liftF $ EvalAsyncEffectInSync asyncEff id


instance Effect' Effect Identity where
  evalConsole consoleAct = liftF $ EvalConsole consoleAct id
  evalIO ioEff = liftF $ EvalIOEff (L.evalIO' ioEff) id

instance Effect' AsyncEffect T.Async where
  evalConsole consoleAct = liftF $ EvalConsole consoleAct id
  evalIO ioEff = liftF $ EvalIOEff (L.evalIO' ioEff) id

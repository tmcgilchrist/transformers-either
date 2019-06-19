{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Except,Extra
-- Copyright   :  (C) 2017 Tim McGilchrist
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  timmcgil@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This monad transformer extends "Control.Monad.Trans.Except" with a few
-- more conveniences.
-----------------------------------------------------------------------------
module Control.Monad.Trans.Except.Extra (
  -- * Control.Monad.Trans.Except.Extra
    newExceptT
  , runExceptT
  , exceptT
  , left
  , right
  , mapExceptT
  , hoistEither
  , bimapExceptT

  -- * Extensions
  , firstExceptT
  , secondExceptT
  , hoistMaybe
  , hoistExceptT
  , handleIOExceptT
  , handleExceptT
  , handlesExceptT
  , handleLeftT

  , catchIOExceptT
  , catchExceptT
  , catchesExceptT
  , catchLeftT

  , bracketExceptT
  , bracketExceptionT
  ) where

import           Control.Exception (Exception, IOException, SomeException)
import qualified Control.Exception as Exception
import           Control.Monad (Monad(..), (=<<))
import           Control.Monad.Catch (Handler (..), MonadCatch, MonadMask, catchAll, mask, throwM)
import qualified Control.Monad.Catch as Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except

import           Data.Maybe (Maybe, maybe)
import           Data.Either (Either(..), either)
import           Data.Foldable (Foldable, foldr)
import           Data.Function (($), (.), const, id, flip)
import           Data.Functor (Functor(..))

import           System.IO (IO)

------------------------------------------------------------------------
-- Control.Monad.Trans.Except.Extra


-- | Constructor for computations in the either monad.
-- (The inverse of 'runExceptT').
newExceptT :: m (Either x a) -> ExceptT x m a
newExceptT =
  ExceptT
{-# INLINE newExceptT #-}

-- | Map over both arguments at the same time.
--
-- Specialised version of 'bimap' for 'ExceptT'.
exceptT :: Monad m => (x -> m b) -> (a -> m b) -> ExceptT x m a -> m b
exceptT f g m =
  either f g =<< runExceptT m
{-# INLINE exceptT #-}

-- | Constructor for left computations.
left :: Monad m => x -> ExceptT x m a
left =
  ExceptT . return . Left
{-# INLINE left #-}

-- | Constructor for right computations.
right :: Monad m => a -> ExceptT x m a
right =
  return
{-# INLINE right #-}

-- | Hoist an 'Either' into an 'ExceptT' m.
hoistEither :: Monad m => Either x a -> ExceptT x m a
hoistEither =
  ExceptT . return
{-# INLINE hoistEither #-}

-- | Map the unwrapped computation using the given function.
bimapExceptT :: Functor m => (x -> y) -> (a -> b) -> ExceptT x m a -> ExceptT y m b
bimapExceptT f g =
  let
    h (Left  e) = Left  (f e)
    h (Right a) = Right (g a)
  in
    mapExceptT (fmap h)
{-# INLINE bimapExceptT #-}

-- | Map the 'Left' unwrapped computation using the given function.
firstExceptT :: Functor m => (x -> y) -> ExceptT x m a -> ExceptT y m a
firstExceptT f =
  bimapExceptT f id
{-# INLINE firstExceptT #-}

-- | Map the 'Right' unwrapped computation using the given function.
secondExceptT :: Functor m => (a -> b) -> ExceptT x m a -> ExceptT x m b
secondExceptT =
  bimapExceptT id
{-# INLINE secondExceptT #-}

-- | Hoist 'Maybe' a into 'Right' a.
hoistMaybe :: Monad m => x -> Maybe a -> ExceptT x m a
hoistMaybe x =
  maybe (left x) return
{-# INLINE hoistMaybe #-}

-- | Hoist 'Except' m into an 'Except' n.
hoistExceptT :: (forall b. m b -> n b) -> ExceptT x m a -> ExceptT x n a
hoistExceptT f =
  ExceptT . f . runExceptT
{-# INLINE hoistExceptT #-}

------------------------------------------------------------------------
-- Error handling

-- | Try an 'IO' action inside an 'ExceptT'. If the 'IO' action throws an
-- 'IOException', catch it and wrap it with the provided handler to convert it
-- to the error type of the 'ExceptT' transformer. Exceptions other than
-- 'IOException' will escape the ExceptT transformer.
--
-- Note: 'IOError' is a type synonym for 'IOException'.
handleIOExceptT :: MonadIO m => (IOException -> x) -> IO a -> ExceptT x m a
handleIOExceptT wrap =
  firstExceptT wrap . newExceptT . liftIO . Exception.try
{-# INLINE handleIOExceptT #-}

-- | Flipped 'handleIOExceptT'.
catchIOExceptT :: MonadIO m => IO a -> (IOException -> x) -> ExceptT x m a
catchIOExceptT = flip handleIOExceptT
{-# INLINE catchIOExceptT #-}

-- | Try any monad action and catch the specified exception, wrapping it to
-- convert it to the error type of the 'ExceptT' transformer. Exceptions other
-- that the specified exception type will escape the 'ExceptT' transformer.
--
-- *Warning*: This function should be used with caution!
-- In particular, it is bad practice to catch 'SomeException' because that
-- includes asynchronous exceptions like stack/heap overflow, thread killed and
-- user interrupt. Trying to handle 'StackOverflow', 'HeapOverflow' and
-- 'ThreadKilled' exceptions could cause your program to crash or behave in
-- unexpected ways.
handleExceptT :: (MonadCatch m, Exception e) => (e -> x) -> m a -> ExceptT x m a
handleExceptT wrap =
  firstExceptT wrap . newExceptT . Catch.try
{-# INLINE handleExceptT #-}

-- | Flipped 'handleExceptT'.
catchExceptT :: (MonadCatch m, Exception e) => m a -> (e -> x) -> ExceptT x m a
catchExceptT = flip handleExceptT
{-# INLINE catchExceptT #-}

-- | Try a monad action and catch any of the exceptions caught by the provided
-- handlers. The handler for each exception type needs to wrap it to convert it
-- to the error type of the 'ExceptT' transformer. Exceptions not explicitly
-- handled by the provided handlers will escape the 'ExceptT' transformer.
handlesExceptT :: (Foldable f, MonadCatch m) => f (Handler m x) -> m a -> ExceptT x m a
handlesExceptT wrappers action =
  newExceptT (fmap Right action `Catch.catch` fmap (fmap Left) handler)
  where
    handler e =
      let probe (Handler h) xs =
            maybe xs h (Exception.fromException e)
      in
        foldr probe (Catch.throwM e) wrappers

-- | Flipped 'handlesExceptT'.
catchesExceptT  :: (Foldable f, MonadCatch m) => m a -> f (Handler m x) -> ExceptT x m a
catchesExceptT = flip handlesExceptT
{-# INLINE catchesExceptT #-}

-- | Handle an error. Equivalent to 'handleError' in mtl package.
handleLeftT :: Monad m => (e -> ExceptT e m a) -> ExceptT e m a -> ExceptT e m a
handleLeftT handler thing = do
  r <- lift $ runExceptT thing
  case r of
    Left e ->
      handler e
    Right a ->
      return a
{-# INLINE handleLeftT #-}

-- | Flipped 'handleLeftT'.
catchLeftT  :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchLeftT = flip handleLeftT
{-# INLINE catchLeftT #-}

-- | Acquire a resource in 'ExceptT' and then perform an action with
-- it, cleaning up afterwards regardless of 'left'.
--
-- This function does not clean up in the event of an exception.
-- Prefer 'bracketExceptionT' in any impure setting.
bracketExceptT :: Monad m => ExceptT e m a -> (a -> ExceptT e m b) -> (a -> ExceptT e m c) -> ExceptT e m c
bracketExceptT before after thing = do
    a <- before
    r <- (\err -> after a >> left err) `handleLeftT` thing a
    -- If handleLeftT already triggered, then `after` already ran *and* we are
    -- in a Left state, so `after` will not run again here.
    _ <- after a
    return r
{-# INLINE bracketExceptT #-}

-- | Acquire a resource in ExceptT and then perform an action with it,
-- cleaning up afterwards regardless of 'left' or exception.
--
-- Like 'bracketExceptT', but the cleanup is called even when the bracketed
-- function throws an exception. Exceptions in the bracketed function are caught
-- to allow the cleanup to run and then rethrown.
bracketExceptionT ::
     MonadMask m
  => ExceptT e m a
  -> (a -> ExceptT e m c)
  -> (a -> ExceptT e m b)
  -> ExceptT e m b
bracketExceptionT acquire release run =
  ExceptT $ bracketF
    (runExceptT acquire)
    (\r -> case r of
      Left _ ->
        -- Acquire failed, we have nothing to release
        return . Right $ ()
      Right r' ->
        -- Acquire succeeded, we need to try and release
        runExceptT (release r') >>= \x -> return $ case x of
          Left err -> Left (Left err)
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Acquire failed, we have nothing to run
        return . Left $ err
      Right r' ->
        -- Acquire succeeded, we can do some work
        runExceptT (run r'))
{-# INLINE bracketExceptionT #-}

-- This is for internal use only. The 'bracketF' function catches all exceptions
-- so the cleanup function can be called and then rethrow the exception.
data BracketResult a =
    BracketOk a
  | BracketFailedFinalizerOk SomeException
  | BracketFailedFinalizerError a

-- Bracket where you care about the output of the finalizer. If the finalizer fails
-- with a value level fail, it will return the result of the finalizer.
-- Finalizer:
--  - Left indicates a value level fail.
--  - Right indicates that the finalizer has a value level success, and its results can be ignored.
--
bracketF :: MonadMask m => m a -> (a -> m (Either b c)) -> (a -> m b) -> m b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk `fmap` g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) `fmap` f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        return b
      BracketOk b -> do
        z <- f a'
        return $ either id (const b) z
{-# INLINE bracketF #-}

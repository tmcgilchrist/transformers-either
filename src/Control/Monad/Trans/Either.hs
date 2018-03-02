{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Either
-- Copyright   :  (C) 2017 Tim McGilchrist
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  timmcgil@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This monad transformer extends "Control.Monad.Trans.Except" with a more
-- familar "Either" naming.
-----------------------------------------------------------------------------
module Control.Monad.Trans.Either (
  -- * Control.Monad.Trans.Either
    EitherT
  , newEitherT
  , pattern EitherT
  , runEitherT
  , eitherT
  , left
  , right
  , mapEitherT
  , hoistEither
  , bimapEitherT

  -- * Extensions
  , firstEitherT
  , secondEitherT
  , hoistMaybe
  , hoistEitherT
  , handleIOEitherT
  , handleEitherT
  , handlesEitherT
  ) where

import           Control.Exception (Exception, IOException)
import qualified Control.Exception as Exception
import           Control.Monad (Monad(..), (=<<))
import           Control.Monad.Catch (MonadCatch, Handler (..))
import qualified Control.Monad.Catch as Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT(..))

import           Data.Maybe (Maybe, maybe)
import           Data.Either (Either(..), either)
import           Data.Foldable (Foldable, foldr)
import           Data.Function ((.), id)
import           Data.Functor (Functor(..))

import           System.IO (IO)

------------------------------------------------------------------------
-- Control.Monad.Trans.Either

-- | Type alias for "ExceptT"
--
type EitherT = ExceptT

pattern EitherT :: m (Either x a) -> ExceptT x m a
pattern EitherT m = ExceptT m

-- | Extractor for computations in the either monad.
-- (The inverse of 'newEitherT').
runEitherT :: EitherT x m a -> m (Either x a)
runEitherT (ExceptT m) = m
{-# INLINE runEitherT #-}

-- | Constructor for computations in the either monad.
-- (The inverse of 'runEitherT').
newEitherT :: m (Either x a) -> EitherT x m a
newEitherT =
  ExceptT
{-# INLINE newEitherT #-}

eitherT :: Monad m => (x -> m b) -> (a -> m b) -> EitherT x m a -> m b
eitherT f g m =
  either f g =<< runEitherT m
{-# INLINE eitherT #-}

-- | Constructor for left computations.
left :: Monad m => x -> EitherT x m a
left =
  EitherT . return . Left
{-# INLINE left #-}

-- | Constructor for right computations.
right :: Monad m => a -> EitherT x m a
right =
  return
{-# INLINE right #-}

-- |
mapEitherT :: (m (Either x a) -> n (Either y b)) -> EitherT x m a -> EitherT y n b
mapEitherT f =
  EitherT . f . runEitherT
{-# INLINE mapEitherT #-}

-- | Hoist an "Either" into an "EitherT m"
hoistEither :: Monad m => Either x a -> EitherT x m a
hoistEither =
  EitherT . return
{-# INLINE hoistEither #-}

-- | Map the unwrapped computation using the given function.
bimapEitherT :: Functor m => (x -> y) -> (a -> b) -> EitherT x m a -> EitherT y m b
bimapEitherT f g =
  let
    h (Left  e) = Left  (f e)
    h (Right a) = Right (g a)
  in
    mapEitherT (fmap h)
{-# INLINE bimapEitherT #-}

-- | Map the 'Left' unwrapped computation using the given function.
firstEitherT :: Functor m => (x -> y) -> EitherT x m a -> EitherT y m a
firstEitherT f =
  bimapEitherT f id
{-# INLINE firstEitherT #-}

-- | Map the 'Right' unwrapped computation using the given function.
secondEitherT :: Functor m => (a -> b) -> EitherT x m a -> EitherT x m b
secondEitherT =
  bimapEitherT id
{-# INLINE secondEitherT #-}

-- | Hoist a 'Maybe a' into a 'Right a'
hoistMaybe :: Monad m => x -> Maybe a -> EitherT x m a
hoistMaybe x =
  maybe (left x) return
{-# INLINE hoistMaybe #-}

-- | Hoist
hoistEitherT :: (forall b. m b -> n b) -> EitherT x m a -> EitherT x n a
hoistEitherT f =
  EitherT . f . runEitherT
{-# INLINE hoistEitherT #-}

-- | Try an `IO` action inside an `EitherT`. If the `IO` action throws an
-- `IOException`, catch it and wrap it with the provided handler to convert it
-- to the error type of the `EitherT` transformer. Exceptions other than
-- `IOException` will escape the EitherT transformer.
-- Note: `IOError` is a type synonym for `IOException`.
handleIOEitherT :: MonadIO m => (IOException -> x) -> IO a -> EitherT x m a
handleIOEitherT wrap =
  firstEitherT wrap . newEitherT . liftIO . Exception.try
{-# INLINE handleIOEitherT #-}

-- | Try any monad action and catch the specified exception, wrapping it to
-- convert it to the error type of the EitherT transformer. Exceptions other
-- that the specified exception type will escape the `EitherT` transformer.
-- _This_function_should_be_used_with_caution_.
-- In particular, it is bad practice to catch SomeException because that
-- includes asynchronous exceptions like stack/heap overflow, thread killed and
-- user interrupt. Trying to handle `StackOverflow`, `HeapOverflow` and
-- `ThreadKilled` exceptions could cause your program to crash.
handleEitherT :: (MonadCatch m, Exception e) => (e -> x) -> m a -> EitherT x m a
handleEitherT wrap =
  firstEitherT wrap . newEitherT . Catch.try
{-# INLINE handleEitherT #-}

-- | Try a monad action and catch any of the exceptions caught by the provided
-- handlers. The handler for each exception type needs to wrap it to convert it
-- to the error type of the `EitherT` transformer. Exceptions not explicitly
-- handled by the provided handlers will escape the `EitherT` transformer.
handlesEitherT :: (Foldable f, MonadCatch m) => f (Handler m x) -> m a -> EitherT x m a
handlesEitherT wrappers action =
  newEitherT (fmap Right action `Catch.catch` fmap (fmap Left) handler)
  where
    handler e =
      let probe (Handler h) xs =
            maybe xs h (Exception.fromException e)
      in
        foldr probe (Catch.throwM e) wrappers

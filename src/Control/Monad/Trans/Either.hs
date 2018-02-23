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
  , bracketEitherT
  ) where

import           Control.Exception (SomeException)
import           Control.Monad (Monad(..), (=<<))
import           Control.Monad.Catch (MonadMask, catchAll, mask, throwM)
import           Control.Monad.Trans.Except (ExceptT(..))

import           Data.Maybe (Maybe, maybe)
import           Data.Either (Either(..), either)
import           Data.Function (($), (.), const, id)
import           Data.Functor (Functor(..))

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

------------------------------------------------------------------------
-- Error handling

-- | Acquire a resource in EitherT and then perform an action with it,
-- cleaning up afterwards regardless of 'left' or exception.
bracketEitherT ::
     MonadMask m
  => EitherT e m a
  -> (a -> EitherT e m c)
  -> (a -> EitherT e m b)
  -> EitherT e m b
bracketEitherT acquire release run =
  EitherT $ bracketF
    (runEitherT acquire)
    (\r -> case r of
      Left _ ->
        -- Acquire failed, we have nothing to release
        return . Right $ ()
      Right r' ->
        -- Acquire succeeded, we need to try and release
        runEitherT (release r') >>= \x -> return $ case x of
          Left err -> Left (Left err)
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Acquire failed, we have nothing to run
        return . Left $ err
      Right r' ->
        -- Acquire succeeded, we can do some work
        runEitherT (run r'))

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

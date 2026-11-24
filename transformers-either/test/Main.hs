{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Exception (ArithException (..), SomeException, throwIO, try)
import           Control.Monad (unless)
import           Control.Monad.Catch (Handler (..))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either

import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           System.Exit (exitFailure)
import           System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ok <- checkSequential tests
  unless ok exitFailure

tests :: Group
tests = Group "Control.Monad.Trans.Either"
  [ ("prop_constructors", prop_constructors)
  , ("prop_handleLeftT", prop_handleLeftT)
  , ("prop_handlesEitherT", prop_handlesEitherT)
  , ("prop_bracketEitherT", prop_bracketEitherT)
  ]

------------------------------------------------------------------------
-- Generators

genErr :: Gen String
genErr = Gen.string (Range.linear 0 10) Gen.alpha

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 (-100) 100)

genEither :: Gen (Either String Int)
genEither = Gen.either genErr genInt

-- | What an action under test does.
data Outcome
  = Return Int
  | ThrowArith
  | ThrowIO
  deriving (Eq, Show)

genOutcome :: Gen Outcome
genOutcome = Gen.choice [Return <$> genInt, pure ThrowArith, pure ThrowIO]

runOutcome :: Outcome -> IO Int
runOutcome o = case o of
  Return n -> return n
  ThrowArith -> throwIO DivideByZero
  ThrowIO -> throwIO (userError "io")

-- | Handlers that can be installed, in a known order.
data HandlerSpec
  = HArith
  | HSome
  deriving (Eq, Show)

toHandler :: HandlerSpec -> Handler IO String
toHandler h = case h of
  HArith -> Handler (\(_ :: ArithException) -> return "arith")
  HSome -> Handler (\(_ :: SomeException) -> return "some")

matches :: HandlerSpec -> Outcome -> Bool
matches h o = case (h, o) of
  (_, Return _) -> False
  (HArith, ThrowArith) -> True
  (HArith, _) -> False
  (HSome, _) -> True

-- | A release action that counts how often it runs, then produces the given
-- result.
countingRelease :: IORef Int -> Either String () -> a -> EitherT String IO ()
countingRelease ref result _ = do
  liftIO (modifyIORef ref (+ 1))
  hoistEither result

-- | Expected result of a bracket whose acquire succeeded: an error from
-- release takes precedence over the body's result.
expectedBracket :: Either String () -> Either String Int -> Either String Int
expectedBracket rel body = either Left (const body) rel

------------------------------------------------------------------------
-- Properties

prop_constructors :: Property
prop_constructors = property $ do
  e <- forAll genErr
  a <- forAll genInt
  ea <- forAll genEither
  ma <- forAll (Gen.maybe genInt)
  r1 <- evalIO $ runEitherT (left e :: EitherT String IO Int)
  r2 <- evalIO $ runEitherT (right a :: EitherT String IO Int)
  r3 <- evalIO $ runEitherT (hoistEither ea :: EitherT String IO Int)
  r4 <- evalIO $ runEitherT (hoistMaybe e ma :: EitherT String IO Int)
  r1 === Left e
  r2 === Right a
  r3 === ea
  r4 === maybe (Left e) Right ma

-- | handleLeftT only invokes the handler on Left, and catchLeftT is its flip.
prop_handleLeftT :: Property
prop_handleLeftT = property $ do
  ea <- forAll genEither
  hr <- forAll genEither
  let handler e = hoistEither (fmap (+ length e) hr) :: EitherT String IO Int
      expected = either (\e -> fmap (+ length e) hr) Right ea
  r1 <- evalIO $ runEitherT (handleLeftT handler (hoistEither ea))
  r2 <- evalIO $ runEitherT (catchLeftT (hoistEither ea) handler)
  r1 === expected
  r2 === expected

-- | handlesEitherT picks the first matching handler, and rethrows any exception
-- no handler matches.
prop_handlesEitherT :: Property
prop_handlesEitherT = property $ do
  outcome <- forAll genOutcome
  specs <- forAll (Gen.list (Range.linear 0 3) (Gen.element [HArith, HSome]))
  r <- evalIO . try $ runEitherT (handlesEitherT (map toHandler specs) (runOutcome outcome))
  let expected = case outcome of
        Return n -> Just (Right n)
        _ -> case filter (`matches` outcome) specs of
          (HArith : _) -> Just (Left "arith")
          (HSome : _) -> Just (Left "some")
          [] -> Nothing
  case r of
    Right v -> Just v === expected
    Left (_ :: SomeException) -> Nothing === expected

-- | bracketEitherT releases exactly once iff acquire succeeded, whatever the
-- body returns.
prop_bracketEitherT :: Property
prop_bracketEitherT = property $ do
  acq <- forAll (Gen.either genErr (pure ()))
  body <- forAll genEither
  rel <- forAll (Gen.either genErr (pure ()))
  ref <- evalIO (newIORef 0)
  r <- evalIO $ runEitherT
    (bracketEitherT (hoistEither acq) (countingRelease ref rel) (\_ -> hoistEither body))
  n <- evalIO (readIORef ref)
  case acq of
    Left e -> do
      r === Left e
      n === 0
    Right () -> do
      r === expectedBracket rel body
      n === 1

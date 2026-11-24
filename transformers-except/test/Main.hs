{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Exception (ArithException (..), SomeException, throwIO, try)
import           Control.Monad (unless)
import           Control.Monad.Catch (Handler (..))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra

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
tests = Group "Control.Monad.Trans.Except.Extra"
  [ ("prop_constructors", prop_constructors)
  , ("prop_handleLeftT", prop_handleLeftT)
  , ("prop_handlesExceptT", prop_handlesExceptT)
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

------------------------------------------------------------------------
-- Properties

prop_constructors :: Property
prop_constructors = property $ do
  e <- forAll genErr
  a <- forAll genInt
  ea <- forAll genEither
  ma <- forAll (Gen.maybe genInt)
  r1 <- evalIO $ runExceptT (left e :: ExceptT String IO Int)
  r2 <- evalIO $ runExceptT (right a :: ExceptT String IO Int)
  r3 <- evalIO $ runExceptT (hoistEither ea :: ExceptT String IO Int)
  r4 <- evalIO $ runExceptT (hoistMaybe e ma :: ExceptT String IO Int)
  r1 === Left e
  r2 === Right a
  r3 === ea
  r4 === maybe (Left e) Right ma

-- | handleLeftT only invokes the handler on Left, and catchLeftT is its flip.
prop_handleLeftT :: Property
prop_handleLeftT = property $ do
  ea <- forAll genEither
  hr <- forAll genEither
  let handler e = hoistEither (fmap (+ length e) hr) :: ExceptT String IO Int
      expected = either (\e -> fmap (+ length e) hr) Right ea
  r1 <- evalIO $ runExceptT (handleLeftT handler (hoistEither ea))
  r2 <- evalIO $ runExceptT (catchLeftT (hoistEither ea) handler)
  r1 === expected
  r2 === expected

-- | handlesExceptT picks the first matching handler, and rethrows any exception
-- no handler matches.
prop_handlesExceptT :: Property
prop_handlesExceptT = property $ do
  outcome <- forAll genOutcome
  specs <- forAll (Gen.list (Range.linear 0 3) (Gen.element [HArith, HSome]))
  r <- evalIO . try $ runExceptT (handlesExceptT (map toHandler specs) (runOutcome outcome))
  let expected = case outcome of
        Return n -> Just (Right n)
        _ -> case filter (`matches` outcome) specs of
          (HArith : _) -> Just (Left "arith")
          (HSome : _) -> Just (Left "some")
          [] -> Nothing
  case r of
    Right v -> Just v === expected
    Left (_ :: SomeException) -> Nothing === expected

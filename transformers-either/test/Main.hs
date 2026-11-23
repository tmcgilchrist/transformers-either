{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad (unless)
import           Control.Monad.Trans.Either

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
  ]

------------------------------------------------------------------------
-- Generators

genErr :: Gen String
genErr = Gen.string (Range.linear 0 10) Gen.alpha

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 (-100) 100)

genEither :: Gen (Either String Int)
genEither = Gen.either genErr genInt

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

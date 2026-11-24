{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Exception (ArithException (..), IOException, SomeException, throwIO, try)
import           Control.Monad (unless)
import           Control.Monad.Catch (Handler (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Functor.Identity (Identity (..))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra

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
tests = Group "Control.Monad.Trans.Except.Extra"
  [ ("prop_constructors", prop_constructors)
  , ("prop_handleLeftT", prop_handleLeftT)
  , ("prop_handlesExceptT", prop_handlesExceptT)
  , ("prop_bracketExceptT", prop_bracketExceptT)
  , ("prop_bracketExceptionT", prop_bracketExceptionT)
  , ("prop_bracketExceptionT_base_short_circuit", prop_bracketExceptionT_base_short_circuit)
  , ("prop_mapping", prop_mapping)
  , ("prop_handleExceptT", prop_handleExceptT)
  , ("prop_hushM_onLeft_onNothing", prop_hushM_onLeft_onNothing)
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
countingRelease :: MonadIO m => IORef Int -> Either String () -> a -> ExceptT String m ()
countingRelease ref result _ = do
  liftIO (modifyIORef ref (+ 1))
  hoistEither result

-- | Expected result of a bracket whose acquire succeeded: an error from
-- release takes precedence over the body's result.
expectedBracket :: Either String () -> Either String Int -> Either String Int
expectedBracket rel body = either Left (const body) rel

-- | What the body of a bracket does.
data BodyOutcome
  = BodyEither (Either String Int)
  | BodyThrow
  deriving (Eq, Show)

genBodyOutcome :: Gen BodyOutcome
genBodyOutcome = Gen.choice [BodyEither <$> genEither, pure BodyThrow]

runBody :: BodyOutcome -> ExceptT String IO Int
runBody b = case b of
  BodyEither ea -> hoistEither ea
  BodyThrow -> liftIO (throwIO DivideByZero)

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

-- | bracketExceptT releases exactly once iff acquire succeeded, whatever the
-- body returns.
prop_bracketExceptT :: Property
prop_bracketExceptT = property $ do
  acq <- forAll (Gen.either genErr (pure ()))
  body <- forAll genEither
  rel <- forAll (Gen.either genErr (pure ()))
  ref <- evalIO (newIORef (0 :: Int))
  r <- evalIO $ runExceptT
    (bracketExceptT (hoistEither acq) (countingRelease ref rel) (\_ -> hoistEither body))
  n <- evalIO (readIORef ref)
  case acq of
    Left e -> do
      r === Left e
      n === 0
    Right () -> do
      r === expectedBracket rel body
      n === 1

-- | bracketExceptionT releases exactly once iff acquire succeeded, including
-- when the body throws, and rethrows exceptions from the body (as its Haddock
-- documents).
prop_bracketExceptionT :: Property
prop_bracketExceptionT = property $ do
  acq <- forAll (Gen.either genErr (pure ()))
  body <- forAll genBodyOutcome
  rel <- forAll (Gen.either genErr (pure ()))
  ref <- evalIO (newIORef (0 :: Int))
  r <- evalIO . try $ runExceptT
    (bracketExceptionT (hoistEither acq) (countingRelease ref rel) (\_ -> runBody body))
  n <- evalIO (readIORef ref)
  case acq of
    Left e -> do
      r === Right (Left e)
      n === 0
    Right () -> do
      n === 1
      case body of
        BodyThrow -> r === Left DivideByZero
        BodyEither ea -> r === Right (expectedBracket rel ea)

-- | bracketExceptionT documents that it cleans up "regardless of 'left' or
-- exception". That must include the base monad short-circuiting (here
-- MaybeT returning Nothing).
prop_bracketExceptionT_base_short_circuit :: Property
prop_bracketExceptionT_base_short_circuit = property $ do
  body <- forAll (Gen.maybe genEither)
  rel <- forAll (Gen.either genErr (pure ()))
  ref <- evalIO (newIORef (0 :: Int))
  let run = maybe (lift (MaybeT (return Nothing))) hoistEither body
             :: ExceptT String (MaybeT IO) Int
  r <- evalIO . runMaybeT $ runExceptT
    (bracketExceptionT (right ()) (countingRelease ref rel) (\_ -> run))
  n <- evalIO (readIORef ref)
  n === 1
  r === fmap (expectedBracket rel) body

-- | The mapping combinators agree with their pure 'Either' counterparts.
prop_mapping :: Property
prop_mapping = property $ do
  ea <- forAll genEither
  let m = hoistEither ea :: ExceptT String Identity Int
      f = reverse
      g = (* 2)
      run = runIdentity . runExceptT
      bimapE f' g' = either (Left . f') (Right . g')
  run (bimapExceptT f g m) === bimapE f g ea
  run (firstExceptT f m) === bimapE f id ea
  run (secondExceptT g m) === bimapE id g ea
  run (mapExceptT (fmap (bimapE f g)) m) === bimapE f g ea
  runIdentity (exceptT (return . length) (return . g) m) === either length g ea
  r <- evalIO $ runExceptT (hoistExceptT (return . runIdentity) m)
  r === ea

-- | handleIOExceptT and handleExceptT catch exactly the requested exception
-- type and let everything else escape. The catch variants are their flips.
prop_handleExceptT :: Property
prop_handleExceptT = property $ do
  outcome <- forAll genOutcome
  let action = runOutcome outcome
      expectIO = case outcome of
        Return n -> Just (Right n)
        ThrowIO -> Just (Left "io")
        ThrowArith -> Nothing
      expectArith = case outcome of
        Return n -> Just (Right n)
        ThrowArith -> Just (Left "arith")
        ThrowIO -> Nothing
      wrapIO = const "io" :: IOException -> String
      wrapArith = const "arith" :: ArithException -> String
      toMaybe :: Either SomeException a -> Maybe a
      toMaybe = either (const Nothing) Just
  r1 <- evalIO . try $ runExceptT (handleIOExceptT wrapIO action :: ExceptT String IO Int)
  r2 <- evalIO . try $ runExceptT (catchIOExceptT action wrapIO :: ExceptT String IO Int)
  r3 <- evalIO . try $ runExceptT (handleExceptT wrapArith action)
  r4 <- evalIO . try $ runExceptT (catchExceptT action wrapArith)
  toMaybe r1 === expectIO
  toMaybe r2 === expectIO
  toMaybe r3 === expectArith
  toMaybe r4 === expectArith

-- | hushM, onLeft and onNothing run their handler exactly when the value is
-- Left / Nothing, and pass Right / Just values through.
prop_hushM_onLeft_onNothing :: Property
prop_hushM_onLeft_onNothing = property $ do
  ea <- forAll genEither
  ma <- forAll (Gen.maybe genInt)
  ref <- evalIO (newIORef (0 :: Int))
  let bump = modifyIORef ref (+ 1)
  r1 <- evalIO $ hushM ea (const bump)
  r2 <- evalIO $ onLeft (\e -> bump >> return (length e)) (return ea)
  r3 <- evalIO $ onNothing (bump >> return 0) (return ma)
  n <- evalIO (readIORef ref)
  r1 === either (const Nothing) Just ea
  r2 === either length id ea
  r3 === maybe 0 id ma
  -- hushM and onLeft each bump once on Left; onNothing bumps once on Nothing.
  n === 2 * either (const 1) (const 0) ea + maybe 1 (const 0) ma
